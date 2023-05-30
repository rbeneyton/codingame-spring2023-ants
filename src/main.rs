#![allow(dead_code)]
#![allow(unused_imports)]

// #![recursion_limit="512"]
use std::io::prelude::*;
use rand::prelude::*;
use std::fmt;
use std::collections::HashMap;

use std::io;

#[macro_use]
pub mod main {
    // {{{ public dependency stuff

    pub use std::cmp;
    pub use std::mem;
    pub use std::mem::MaybeUninit;
    pub use std::mem::size_of;
    pub use std::io::Write; // for flush
    pub use std::time::Instant;
    pub use std::fmt::Display;
    pub use std::fmt::Formatter;
    pub use std::ops::Index;
    pub use std::ops::IndexMut;
    pub use std::ops::Add;
    pub use std::ops::AddAssign;
    pub use std::ops::BitAnd;
    pub use std::ops::BitAndAssign;
    pub use std::ops::BitOr;
    pub use std::ops::BitOrAssign;
    pub use std::ops::Not;
    pub use std::convert::Into;
    pub use std::convert::TryInto;
    pub use std::convert::From;
    pub use std::convert::TryFrom;
    pub use std::collections::HashSet;
    pub use std::collections::HashMap;
    pub use std::collections::hash_map::Entry;
    pub use std::collections::BinaryHeap;
    pub use std::hash::{Hash, Hasher};
    pub use std::collections::hash_map::DefaultHasher;
    pub use std::cell::{Cell, RefCell};
    #[cfg(feature = "customizable")]
    pub use rand::SeedableRng;
    #[cfg(feature = "customizable")]
    pub use rand::rngs::SmallRng;
    pub use itertools::Itertools; // format(), join() functions
    pub use crate::*;

    // }}}
    // {{{ game parameters and constants

    // turn type
    pub type Turn = u16;
    // final turn
    pub const MAX_TURN: Turn = 100;

    // generic index types
    pub type Idx = u8;
    // our invalid index constant, should not be 0
    pub const INVALID_IDX: Idx = Idx::MAX;

    // estimated score constant
    pub type Score = u16;

    pub const MAX_STARTUP_DURATION : u128 = 900_000; // 1000ms with a security

    pub const MAX_TURN_DURATION : u128 = 95_000; // 100ms with a security

    // hexadecimal world
    pub const NEIGH_SIZE: usize = 6;
    // forest max tree position
    pub const MAP_SIZE: usize = 49; // 38;
    pub const MAP_SIZE_2: usize = MAP_SIZE * MAP_SIZE;
    // max step for neighbors
    pub const MAX_STEP: usize = 4;

    pub const MAX_ACTIONS: usize = 64;
        // (256 - size_of::<usize>()) / size_of::<Action>(); // 82
    pub const MAX_HISTORY: usize = (MAX_TURN as usize) + 1; // 1 << 10;
    pub const MAX_CANDS: usize = 31;

    // mem pool?
    pub const NODES_POOL_SZ : usize = 1 << 20; // 1M simultaneous states

    pub const MESSAGES: [&'static str; 11] = [
        "かわいい", // kawaii. cute, pretty, adorable. Basically, kawaii means “cute” or “pretty”. ...
        "キモい", // kimoi. disgusting, gross. ...
        "ウザい", // uzai. annoying, irritating. ...
        "ムカつく", // mukatsuku. pissed off. ...
        "すごい", // sugoi. awesome, terrific, very. ...
        "マジ",  // maji. seriously. ...
        "ヤバい", // yabai. oh my gosh, wow. ...
        "超めっちゃ", // choo/meccha. so, super.
        "ばか", // (baka): Idiot.
        "ダサい", // (dasai): Lame, out of style, dorky, or sucky.
        "お前", // (omae): A rude, blunt way to say “you.” It's mostly used by men to other men as an insult because it literally means “The thing in front of me.” So, this person is so lowly they're just this thing in your way.
    ];

    // }}}
    // {{{ logging

    static mut LOG_LVL : u8 = 0;

    // enable assert
    pub const ENABLE_LOG : bool = true;

    // force echo mode
    pub const FORCE_ECHO : bool = true;

    // enable assert
    pub const ENABLE_ASSERT : bool = true;

    pub fn is_log_enable() -> bool {
        ENABLE_LOG || cfg!(customizable)
    }
    pub fn set_log_lvl(lvl: u8) {
        if is_log_enable() {
            unsafe {
                LOG_LVL = lvl;
            }
        } else {
            // NOTHING
        }
    }
    pub fn log_lvl() -> u8 {
        if is_log_enable() {
            unsafe {
                LOG_LVL
            }
        } else {
            0
        }
    }
    #[macro_export]
    macro_rules! logstart {
        ($lvl:expr, $($arg:tt)+) => ({
            if is_log_enable() {
                let lvl = $lvl;
                if lvl <= log_lvl() {
                    eprint!("L{}:L{}:{}",
                        line!(),
                        lvl,
                        fmt::format(format_args!($($arg)+)));
                }
            } else {
                // NOTHING
            }
        })
    }
    #[macro_export]
    macro_rules! logcont {
        ($lvl:expr, $($arg:tt)+) => ({
            if is_log_enable() {
                let lvl = $lvl;
                if lvl <= log_lvl() {
                    eprint!("{}",
                        fmt::format(format_args!($($arg)+)));
                }
            } else {
                // NOTHING
            }
        })
    }
    #[macro_export]
    macro_rules! logstop {
        ($lvl:expr, $($arg:tt)+) => ({
            if is_log_enable() {
                let lvl = $lvl;
                if lvl <= log_lvl() {
                    eprintln!("{}",
                        fmt::format(format_args!($($arg)+)));
                }
            } else {
                // NOTHING
            }
        })
    }
    #[macro_export]
    macro_rules! logln {
        ($lvl:expr, $($arg:tt)+) => ({
            if is_log_enable() {
                let lvl = $lvl;
                if lvl <= log_lvl() {
                    eprintln!("L{}:L{}:{}",
                        line!(),
                        lvl,
                        fmt::format(format_args!($($arg)+)));
                    std::io::stderr().flush().unwrap();
                }
            } else {
                // NOTHING
            }
        })
    }

    // }}}
    // {{{ safe exit handling

    static mut STOP : bool = false;

    pub fn ctrlc_stop() {
        if cfg!(customizable) {
            unsafe {
                STOP = true;
            }
        } else {
            // NOTHING
        }
    }
    pub fn get_ctrlc_stop() -> bool {
        if cfg!(customizable) {
            unsafe {
                STOP
            }
        } else {
            false
        }
    }


    // }}}
    // {{{ utility

    pub fn get_stdin_line(dump_line: bool) -> String {
        use std::io;

        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();
        let line = line.trim_end().to_string();
        if false && dump_line {
            eprintln!("{}", line);
            // logln!(0, "{}", line);
        }
        line
    }

    #[macro_export]
    macro_rules! get_rng {
        ($seed:expr) => ({
            #[cfg(feature = "customizable")]
            {
                let seed = $seed;
                SmallRng::seed_from_u64(seed)
            }
            #[cfg(not(feature = "customizable"))]
            {
                rand::thread_rng()
            }
        })
    }

    // short version of fastrand crate
    pub struct Rng(u64);
    impl Rng {
        pub fn new() -> Rng {
            Rng(0x4d595df4d0f33173)
        }
        #[inline]
        pub fn gen_u32(&mut self) -> u32 {
            let s = self.0;
            self.0 = s.wrapping_mul(6364136223846793005)
                 .wrapping_add(1442695040888963407);
            (((s ^ (s >> 18)) >> 27) as u32).rotate_right((s >> 59) as u32)
        }
        #[inline]
        fn gen_u64(&mut self) -> u64 {
            ((self.gen_u32() as u64) << 32) | (self.gen_u32() as u64)
        }
        #[inline]
        fn gen_usize(&mut self) -> usize {
            self.gen_u64() as usize
        }
    }

    // codingame is now in release mode, so we try to control cg_assert behavior to improve perf
    #[macro_export]
    macro_rules! cg_assert {
        ($($arg:tt)*) => ({
            if main::ENABLE_ASSERT {
                assert!($($arg)*);
            }
        })
    }
    #[macro_export]
    macro_rules! cg_assert_eq {
        ($($arg:tt)*) => ({
            if main::ENABLE_ASSERT {
                assert_eq!($($arg)*);
            }
        })
    }
    // }}}
    // {{{ FixedArray

    #[derive(PartialEq, Eq, Debug, Clone, Copy)]
    pub struct FixedArray<T, const LENGTH: usize> {
        len : usize,
        data : [T; LENGTH], // BUG still direct access from derived types?
    }

    impl<'a, T: Copy + Default, const LENGTH: usize> FixedArray<T, LENGTH> {
        pub fn capacity(&self) -> usize {
            LENGTH
        }
        pub fn full(&self) -> bool {
            self.len == LENGTH
        }
        pub fn append(&mut self, tree: T) {
            if self.len < LENGTH {
                self.data[self.len] = tree;
                self.len += 1;
            } else {
                cg_assert!(false, "append reach maximum LENGTH");
            }
        }
        // TODO MaybeUninit<K>
        pub fn append_sorted<K>(&mut self, o: T, k: K, keys: &mut [K; LENGTH])
            where K: Copy + Default + Ord + Eq + Display,
        {
            for i in 1..self.len {
                cg_assert!(keys[i - 1] >= keys[i]);
            }
            if self.len == 0 {
                self.data[0] = o;
                keys[0] = k;
                self.len = 1;
                return;
            }
            let new_len = if self.full() { LENGTH } else { self.len + 1 };
            for i in 0..self.len {
                if k > keys[i] {
                    // object lost
                    for j in ((i + 1)..new_len).rev() {
                        self.data[j] = self.data[j - 1];
                        keys[j] = keys[j - 1];
                    }
                    self.data[i] = o;
                    keys[i] = k;
                    self.len = new_len;
                    return;
                }
            }
            // terminal case
            if !self.full() {
                self.data[self.len] = o;
                keys[self.len] = k;
                self.len = new_len;
            }
        }
        pub fn remove(&mut self, idx: usize) {
            if idx < self.len - 1 {
                self.data[idx] = self.last();
            }
            self.len -= 1
        }
        pub fn len(&self) -> usize {
            self.len
        }
        pub fn set_len(&mut self, len: usize) {
            self.len = len
        }
        pub fn with_len(len: usize) -> Self {
            Self {
                data: [T::default(); LENGTH],
                len,
            }
        }
        pub fn clear(&mut self) {
            self.len = 0
        }
        pub fn last(&self) -> T {
            self.data[self.len - 1]
        }
        pub fn last2(&self) -> T {
            self.data[self.len - 2]
        }
        pub fn iter(&self) -> IterFixedArray<T, LENGTH> {
            IterFixedArray {
                slice: &self.data,
                len: self.len,
                idx: 0,
            }
        }
    }

    // {{{ Iterator

    pub struct IterFixedArray<'a, T, const LENGTH: usize> {
        slice: &'a [T; LENGTH],
        len: usize, // store Ts len
        idx: usize, // current iterator index
    }
    impl<'a, T, const LENGTH: usize> Iterator for IterFixedArray<'a, T, LENGTH> {
        type Item = &'a T;

        fn next(&mut self) -> Option<Self::Item> {
            if self.idx >= self.len {
                return None;
            }
            let res = &self.slice[self.idx];
            self.idx += 1;
            Some(res)
        }
    }

    impl<'a, T, const LENGTH: usize> IterFixedArray<'a, T, LENGTH> {
        pub fn position(&self) -> usize {
            self.idx
        }
    }

    // }}}
    // {{{ IndexOf

    impl<T: Copy + Default, const LENGTH: usize> Index<usize> for FixedArray<T, LENGTH> {
        type Output = T;

        fn index(&self, idx: usize) -> &Self::Output {
            cg_assert!(idx < self.len, "FixedArray Index bound access {}/{} capacity:{}",
                idx, self.len, LENGTH);
            &self.data[idx]
        }
    }

    impl<T: Copy + Default, const LENGTH: usize> IndexMut<usize> for FixedArray<T, LENGTH> {
        fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
            cg_assert!(idx < self.len, "FixedArray IndexMut bound access {}/{} capacity:{}",
                idx, self.len, LENGTH);
            &mut self.data[idx]
        }
    }

    // }}}
    // {{{ Default + Display

    impl<T: Copy + Default, const LENGTH: usize> Default for FixedArray<T, LENGTH> {
        fn default() -> Self {
            Self {
                data: [T::default(); LENGTH],
                len: 0,
            }
        }
    }

    impl<T: Copy + Default + Display, const LENGTH: usize> Display for FixedArray<T, LENGTH> {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "{}", itertools::join(self.iter(), ",")).unwrap();
            Ok(())
        }
    }

    // }}}
    // }}}
    // {{{ SortedFixedArray

    #[derive(PartialEq, Eq, Debug, Clone, Copy)]
    pub struct SortedFixedArray<T, K, const LENGTH: usize>
    {
        data : [T; LENGTH], // BUG still direct access from derived types?
        keys: [K; LENGTH], // BUG still direct access from derived types?
        len : usize,
    }

    impl<'a, T, K, const LENGTH: usize> SortedFixedArray<T, K, LENGTH>
    where T: Copy + Default,
          K: Copy + Default + PartialOrd + PartialEq + Display,
    {
        pub fn capacity(&self) -> usize {
            LENGTH
        }
        pub fn full(&self) -> bool {
            self.len == LENGTH
        }
        pub fn append(&mut self, o: T, k: K) {
            for i in 1..self.len {
                cg_assert!(self.keys[i - 1] >= self.keys[i]);
            }
            if self.len == 0 {
                self.data[0] = o;
                self.keys[0] = k;
                self.len = 1;
                return;
            }
            let new_len = if self.full() { LENGTH } else { self.len + 1 };
            for i in 0..self.len {
                if k > self.keys[i] {
                    // object lost
                    for j in ((i + 1)..new_len).rev() {
                        self.data[j] = self.data[j - 1];
                        self.keys[j] = self.keys[j - 1];
                    }
                    self.data[i] = o;
                    self.keys[i] = k;
                    self.len = new_len;
                    return;
                }
            }
            // terminal case
            if !self.full() {
                self.data[self.len] = o;
                self.keys[self.len] = k;
                self.len = new_len;
            }
        }
        pub fn len(&self) -> usize {
            self.len
        }
        pub fn with_len(len: usize) -> Self {
            Self {
                data: [T::default(); LENGTH],
                keys: [K::default(); LENGTH],
                len,
            }
        }
        pub fn clear(&mut self) {
            self.len = 0
        }
        pub fn last(&self) -> T {
            self.data[self.len - 1]
        }
        pub fn last2(&self) -> T {
            self.data[self.len - 2]
        }
    }

    impl<'a, T, K, const LENGTH: usize> SortedFixedArray<T, K, LENGTH>
    where T: Copy + Default + Display,
          K: Copy + Default,
    {
        pub fn iter(&self) -> IterFixedArray<T, LENGTH> {
            IterFixedArray {
                slice: &self.data,
                len: self.len,
                idx: 0,
            }
        }
    }
    // {{{ Iterator

    pub struct IterSortedFixedArray<'a, T, const LENGTH: usize> {
        slice: &'a [T; LENGTH],
        len: usize, // store Ts len
        idx: usize, // current iterator index
    }
    impl<'a, T, const LENGTH: usize> Iterator for IterSortedFixedArray<'a, T, LENGTH> {
        type Item = &'a T;

        fn next(&mut self) -> Option<Self::Item> {
            if self.idx >= self.len {
                return None;
            }
            let res = &self.slice[self.idx];
            self.idx += 1;
            Some(res)
        }
    }

    impl<'a, T, const LENGTH: usize> IterSortedFixedArray<'a, T, LENGTH> {
        pub fn position(&self) -> usize {
            self.idx
        }
    }

    // }}}
    // {{{ IndexOf

    impl<T, K, const LENGTH: usize> Index<usize> for SortedFixedArray<T, K, LENGTH>
    where T: Copy + Default,
          K: Copy + Default,
    {
        type Output = T;

        fn index(&self, idx: usize) -> &Self::Output {
            cg_assert!(idx < self.len, "FixedArray Index bound access {}/{} capacity:{}",
                idx, self.len, LENGTH);
            &self.data[idx]
        }
    }

    impl<T, K, const LENGTH: usize> IndexMut<usize> for SortedFixedArray<T, K, LENGTH>
    where T: Copy + Default + Display,
          K: Copy + Default,
    {
        fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
            cg_assert!(idx < self.len, "FixedArray IndexMut bound access {}/{} capacity:{}",
                idx, self.len, LENGTH);
            &mut self.data[idx]
        }
    }

    // }}}
    // {{{ Default + Display

    impl<T, K, const LENGTH: usize> Default for SortedFixedArray<T, K, LENGTH>
    where T: Copy + Default,
          K: Copy + Default,
    {
        fn default() -> Self {
            Self {
                data: [T::default(); LENGTH],
                keys: [K::default(); LENGTH],
                len: 0,
            }
        }
    }

    impl<T, K, const LENGTH: usize> Display for SortedFixedArray<T, K, LENGTH>
    where T: Copy + Default + Display,
          K: Copy + Default,
    {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "{}", itertools::join(self.iter(), ",")).unwrap();
            Ok(())
        }
    }

    // }}}
    // }}}
    // {{{ BotOptions

    #[derive(Debug, Clone)]
    pub struct BotOptions {
    }

    // }}}
    // {{{ Options

    #[derive(Debug, Clone)]
    pub struct MyOptions {
        pub verbose: u8,

        pub bot: bool,
        pub bot_policy: Policy,
        pub opp_policy: Policy,

        pub simulation: bool, // true: simulate a game
        pub learn: bool, // true: learn mode (optimize itself agains opp bot)

        // variable parameters of bot (estimated score computation + …)

        // beam search width
        beam_width: usize,

        // tuning mode
        pub randomize: bool,

        // simulation related
        pub game_n: isize, // if -1, loop forever
        pub seed: u64, // random seed
    }

    impl MyOptions {
        // {{{

        const BEAM_WIDTH: usize = 3000;

        #[cfg(feature = "customizable")]
        pub fn new() -> Self {
            let mut res = Self::default();

            let args: Vec<String> = std::env::args().collect();

            use getopts::Options;

            let mut opts = Options::new();

            opts.optflag("h", "help", "");
            opts.optopt("v", "verbose", "verbose level (0-100)", "VERBOSE");

            opts.optflag("s", "simulation", "simulation mode");
            opts.optflag("l", "learn", "learn mode");
            opts.optflag("b", "bot", "use a bot");
            opts.optopt("p", "policy",
                format!("bot policy ({})",
                        Policy::VALUES.iter().format(", ")).as_str(),
                "BOT_POLICY");
            opts.optopt("o", "opp-policy",
                format!("opp bot policy ({})",
                        Policy::VALUES.iter().format(", ")).as_str(),
                "BOT_POLICY");

            opts.optopt("n", "game", "number of game to play", "GAME");
            opts.optopt("s", "seed", "random seed", "SEED");
            opts.optflag("r", "randomize", "random options of bots");

            let desc = "spring chalenge 2023";

            let matches = match opts.parse(&args[1..]) {
                Ok(m) => m,
                Err(err) => {
                    println!("{}\n{}", err, opts.usage(desc));
                    std::process::exit(2);
                },
            };

            if matches.opt_present("h") {
                println!("{}", opts.usage(desc));
                std::process::exit(2);
            }

            if let Some(opt) = matches.opt_str("verbose") {
                match opt.parse::<u8>() {
                    Ok(v) => {
                        res.verbose = v;
                    },
                    Err(err) => {
                        println!("{}\n{}", err, opts.usage(desc));
                        std::process::exit(2);
                    },
                }
            }
            set_log_lvl(res.verbose);
            res.simulation = matches.opt_present("s");
            res.learn = matches.opt_present("l");
            if res.learn && res.simulation {
                println!("Both learn and simulation mode at same time is impossible");
                std::process::exit(2);
            }
            res.bot = matches.opt_present("b");
            res.randomize = matches.opt_present("r");
            if let Some(opt) = matches.opt_str("p") {
                res.bot_policy = Policy::new(&opt);
            }
            if let Some(opt) = matches.opt_str("o") {
                res.opp_policy = Policy::new(&opt);
            }
            if let Some(game_n) = matches.opt_str("n") {
                res.game_n = game_n.parse::<isize>().unwrap();
            }
            if let Some(seed) = matches.opt_str("s") {
                res.seed = seed.parse::<u64>().unwrap();
            }

            res
        }

        #[cfg(not(feature = "customizable"))]
        pub fn new() -> Self {
            Self::default()
        }

        pub fn dump_parameters_names(&self, suffix: &str) {
            print!("{}{},", "beam_width"                , suffix);
        }
        pub fn dump_parameters(&self) {
            print!("{},", self.beam_width);
        }
        pub fn shuffle<R: rand::Rng>(&mut self, rng: &mut R) {
            let scan_width = 10;
            let mut uniform = || (rng.gen::<usize>() % scan_width);

            self.beam_width                 = Self::BEAM_WIDTH -3000 + 6000 * uniform() as usize;
        }

        pub fn get_beam_width(&self) -> usize {
            if cfg!(customizable) { self.beam_width } else { Self::BEAM_WIDTH }
        }
        // }}}
    }

    impl Default for MyOptions {
        fn default() -> Self {
            Self {
                verbose: log_lvl(),
                bot: true,
                //bot_policy: Policy::Echo, // XXX
                bot_policy: Policy::LinesSmart, // XXX
                opp_policy: Policy::Scan,
                simulation: false,
                learn: false,

                beam_width: Self::BEAM_WIDTH,

                randomize: false,

                game_n: 1,
                seed: 23,
            }
        }
    }

    impl fmt::Display for MyOptions {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            writeln!(f, "verbose: {}", self.verbose).unwrap();
            writeln!(f, "bot: {}", self.bot).unwrap();
            writeln!(f, "bot_policy: {}", self.bot_policy).unwrap();
            writeln!(f, "opp_policy: {}", self.opp_policy).unwrap();
            writeln!(f, "simulation: {}", self.simulation).unwrap();
            writeln!(f, "learn: {}", self.learn).unwrap();
            Ok(())
        }
    }

    // }}}
    // {{{ MCell

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum CellType {
        Empty,
        Egg,
        Crystal,
    }
    impl Default for CellType {
        fn default() -> Self {
            Self::Empty
        }
    }
    impl From<&str> for CellType {
        fn from(o: &str) -> Self {
            match o.parse::<usize>() {
                Ok(0) => CellType::Empty,
                Ok(1) => CellType::Egg,
                Ok(2) => CellType::Crystal,
                _ => panic!("invalid CellType {o}"),
            }
        }
    }

    #[derive(Eq, PartialEq, Debug, Clone, Copy)]
    pub struct MCell {
        pub typ: CellType,
        pub initial_resources: Score,
        pub neigh: [Idx; NEIGH_SIZE], // in [0..MAP_SIZE[, so 6 bits
    }

    impl MCell {
        pub fn from_string(input: &String) -> Self {
            // format: <TYPE> <RESSOURCES> 6<NEIGH>
            let input : Vec<&str> = input.split(' ').collect();
            let typ : CellType = input[0].into();
            let initial_resources = input[1].parse::<Score>().unwrap();
            let mut neigh = [INVALID_IDX; NEIGH_SIZE];
            for i in 0..NEIGH_SIZE {
                let v = input[2 + i].parse::<isize>().unwrap();
                // codingame put '-1' for no neighbor
                if v >= 0 {
                    cg_assert!(v < MAP_SIZE as isize, "cell index error when reading");
                    neigh[i] = v as Idx;
                }
            }

            Self {
                typ,
                initial_resources,
                neigh,
            }
        }

        pub fn valid(idx: Idx) -> bool {
            idx != INVALID_IDX
        }
    }

    impl Default for MCell {
        fn default() -> Self {
            Self {
                typ : CellType::default(),
                initial_resources : 0,
                neigh: [INVALID_IDX; NEIGH_SIZE],
            }
        }
    }

    impl fmt::Display for MCell {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "type:{:?} ", self.typ).unwrap();
            write!(f, "initial_resources:{} ", self.initial_resources).unwrap();
            write!(f, "neigh ").unwrap();
            for i in 0..NEIGH_SIZE {
                write!(f, "{} ", self.neigh[i]).unwrap();
            }
            Ok(())
        }
    }

    // }}}
    // {{{ IdxMask

    // struct to pack bit and maintain count
    #[derive(Default, Eq, PartialEq, Debug, Clone, Copy, Hash)]
    pub struct IdxMask {
        pub data : u64,
    }

    // Index & IndexMut not possible here with their API by reference /o\

    impl IdxMask {
        pub const MAX : Idx = MAP_SIZE as Idx;
        pub const DATA_MASK : u64 = (1u64 << MAP_SIZE) - 1;
        pub const COUNT_OFFSET : Score = 64 - 8;
        pub const DIRTY : u64 = (1u64 << (64 - 16)); // means count is not up-to-date

        pub fn new(data : u64) -> Self{
            Self {
                data,
            }
        }
        // XXX add/remove/... does not update count!
        pub fn add(&mut self, idx: Idx) {
            self.data |= 1u64 << idx;
        }
        pub fn remove(&mut self, idx: Idx) {
            self.data &= !(1u64 << idx);
        }
        pub fn get(&self, idx: Idx) -> bool {
            (self.data & 1u64 << idx) != 0
        }
        pub fn set_count(&mut self, count: Idx) {
            self.data |= (count as u64) << Self::COUNT_OFFSET;
        }
        pub fn dirty(&self) -> bool {
            (self.data & Self::DIRTY) != 0
        }
        pub fn count_raw(&self) -> Idx {
            cg_assert!(!self.dirty(), "geting count of an outdated field");
            (self.data >> Self::COUNT_OFFSET) as Idx
        }
        pub fn count(&mut self) -> Idx {
            if self.dirty() {
                self.update_count();
            }
            (self.data >> Self::COUNT_OFFSET) as Idx
        }
        pub fn update_count(&mut self) {
            self.data &= Self::DATA_MASK;
            let count = self.data.count_ones() as Idx;
            self.set_count(count);
        }
        pub fn iter(&self) -> IterIdxMask {
            IterIdxMask {
                data: self.data,
                idx: Idx::MAX,
            }
        }
        pub fn dump_with<F>(_f: F) -> String
        where F: Fn(Idx) -> char,
        {
            let res = String::new();
            // for line in Forest::LINES.iter() {
            //     for id in line.iter() {
            //         if *id < 0 {
            //             res.push_str(" ");
            //         } else {
            //             let id = *id as Idx;
            //             res.push(f(id));
            //         }
            //     }
            //     res.push_str("\n");
            // }
            res
        }
    }

    // {{{ Iterator

    pub struct IterIdxMask {
        data: u64, // full copy
        idx: Idx,
    }
    impl Iterator for IterIdxMask {
        type Item = Idx;

        fn next(&mut self) -> Option<Self::Item> {
            loop {
                self.idx = self.idx.wrapping_add(1);
                if self.idx == IdxMask::MAX {
                    return None;
                }
                if (self.data & 1u64 << self.idx) != 0 {
                    return Some(self.idx);
                }
            }
        }
    }

    // }}}
    // {{{ Arithmetic

    impl BitAnd for IdxMask {
        type Output = Self;

        fn bitand(self, rhs: Self) -> Self::Output {
            Self::new(self.data & (rhs.data & Self::DATA_MASK))
        }
    }

    impl BitAndAssign for IdxMask {
        fn bitand_assign(&mut self, rhs: Self) {
            self.data &= rhs.data & Self::DATA_MASK;
        }
    }

    impl BitOr for IdxMask {
        type Output = Self;

        fn bitor(self, rhs: Self) -> Self::Output {
            Self::new(self.data | (rhs.data & Self::DATA_MASK))
        }
    }

    impl BitOrAssign for IdxMask {
        fn bitor_assign(&mut self, rhs: Self) {
            self.data |= rhs.data & Self::DATA_MASK;
        }
    }

    impl Not for IdxMask {
        type Output = Self;

        fn not(self) -> Self::Output {
            Self::new(!(self.data & Self::DATA_MASK))
        }
    }

    // }}}
    // {{{ Display

    impl fmt::Display for IdxMask {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "[{:2}]{:#064b}", self.count_raw(), self.data).unwrap();
            Ok(())
        }
    }

    // }}}
    // }}}
    // {{{ Map

    // otherwise no default on > 32
    pub type Cells = FixedArray::<MCell, MAP_SIZE>;
    pub type IdxMasks = FixedArray::<IdxMask, MAP_SIZE>;
    pub type Distances = FixedArray::<u8, MAP_SIZE_2>;

    #[derive(Default, Eq, PartialEq, Debug, Clone, Copy)]
    pub struct Map {
        pub nc : usize, // numberOfCells
        pub cells : Cells,
        pub nb : usize, // numberOfBases
        pub nb_allied : Idx, // numberOfBases Allied
        pub nb_opp : Idx, // numberOfBases Opponent

        // computed
        pub initial_resources : Score,

        // n step neighbors
        // bitmask of all n step neigbors
        // - first index is number of step in [1,2,3] ('0' is not used)
        // - second index is cell idx
        pub neig : [IdxMasks; MAX_STEP], // bitmask of all n step neigbors
        // idem but without hole
        pub zone : [IdxMasks; MAX_STEP], // bitmask of all n step neigbors

        // distance matrix
        pub distances : Distances,

        // ressources > 0 cells
        pub resources : IdxMask,
    }

    impl Map {
        // {{{

        pub fn init(&mut self) {
        }

        pub fn from_string(&mut self, lines: &Vec<String>) {
            // {{{ parsing

            // format:
            // line 1: numberOfCells
            // line (numberOfCells): cell
            // line 3: numberOfBases
            // line 4: numberOfBases for allies
            // line 5: numberOfBases for opps
            let mut lineno = 0;

            self.nc = lines[lineno].parse::<usize>().unwrap();
            lineno += 1;
            cg_assert!(lines.len() >= lineno + self.nc);
            self.cells = Cells::with_len(self.nc);
            for idx in 0..(self.nc) {
                cg_assert!(idx < self.cells.capacity());
                self.cells[idx] = MCell::from_string(&lines[lineno]);
                lineno += 1;
            }
            cg_assert!(lines.len() >= lineno + 3);
            self.nb = lines[lineno].parse::<usize>().unwrap();
            lineno += 1;
            cg_assert_eq!(self.nb, 1);
            self.nb_allied = lines[lineno].parse::<Idx>().unwrap();
            lineno += 1;
            self.nb_opp = lines[lineno].parse::<Idx>().unwrap();
            // lineno += 1;

            // }}}
            // {{{ compute extra stuff

            self.initial_resources = (0..(self.nc))
                .map(|x| self.cells[x])
                .filter(|x| x.typ == CellType::Crystal)
                .map(|x| x.initial_resources)
                .sum();

            // BC
            let neig0 = IdxMasks::with_len(self.nc);
            let mut neig1 = IdxMasks::with_len(self.nc);
            let mut neig2 = IdxMasks::with_len(self.nc);
            let mut neig3 = IdxMasks::with_len(self.nc);
            for idx in 0..(self.nc) {
                let cell = &self.cells[idx];
                for nidx in cell.neigh.iter().filter(|x| MCell::valid(**x)) {
                    neig1[idx].add(*nidx);
                }
            }
            for idx in 0..(self.nc) {
                neig2[idx] |= neig1[idx];
                for nidx in neig1[idx].iter() {
                    neig2[idx] |= neig1[nidx as usize];
                }
            }
            for idx in 0..(self.nc) {
                neig3[idx] |= neig1[idx];
                for nidx in neig1[idx].iter() {
                    neig3[idx] |= neig1[nidx as usize];
                }
                neig3[idx] |= neig2[idx];
                for nidx in neig2[idx].iter() {
                    neig3[idx] |= neig1[nidx as usize];
                }
            }
            // build disjoins sets (remove duplicates + own point)
            for idx in 0..(self.nc) {
                neig1[idx].remove(idx as Idx);
                neig2[idx].remove(idx as Idx);
                neig2[idx] &= !neig1[idx];
                neig3[idx].remove(idx as Idx);
                neig3[idx] &= !neig1[idx];
                neig3[idx] &= !neig2[idx];
            }
            // BC with new type IdxMasks (why?)
            self.neig = [
                neig0,
                neig1,
                neig2,
                neig3,
            ];
            for step in 1..MAX_STEP {
                for idx in 0..(self.nc) {
                    self.neig[step][idx].update_count();
                    logln!(99, "{}=>{}", idx, self.neig[step][idx]);
                }
            }
            self.zone = [IdxMasks::with_len(self.nc); MAX_STEP];
            for step in 1..MAX_STEP {
                for substep in 1..=step {
                    for idx in 0..(self.nc) {
                        self.zone[step][idx] |= self.neig[substep][idx];
                    }
                }
                for idx in 0..(self.nc) {
                    self.zone[step][idx].update_count();
                }
            }

            self.resources = IdxMask::default();
            for idx in 0..(self.nc) {
                if self.cells[idx].initial_resources > 0 {
                    self.resources.add(idx as Idx);
                }
            }
            self.resources.update_count();

            // }}}
        }

        fn distance(&self, start : impl Into<usize>, stop : impl Into<usize>) -> Idx {
            let nc = self.nc;
            let start : usize = start.into();
            let stop : usize = stop.into();
            let idx = |a, b| a * nc + b;
            self.distances[idx(start, stop)]
        }

        // {{{ compute_distances

        fn compute_distances(&mut self) {
            let nc = self.nc;
            self.distances.set_len(nc * nc);
            // let idx = |a, b| a * nc + b;
            let mut cells = HashSet::with_capacity(nc);
            let mut next_cells = HashSet::with_capacity(nc);
            let unseen = INVALID_IDX; // bigger maps later?

            // quick & dirty BFS
            for start in 0..nc {
                let idx = |b| start * nc + b;
                // init
                for stop in 0..nc {
                    self.distances[idx(stop)] = unseen;
                }
                cells.clear();
                cells.insert(start);
                self.distances[idx(start)] = 0;
                loop {
                    next_cells.clear();
                    for i in &cells {
                        let cur = self.distances[idx(*i)];
                        let cell = &self.cells[*i];
                        let d = cur + 1;
                        cg_assert!(d < unseen - 1);
                        for j in cell.neigh.iter()
                            .filter(|x| MCell::valid(**x))
                            .map(|x| *x as usize)
                        {
                            if self.distances[idx(j)] == unseen || self.distances[idx(j)] > d {
                                self.distances[idx(j)] = d;
                                next_cells.insert(j);
                            }
                        }
                    }
                    if next_cells.len() == 0 {
                        break;
                    }
                    std::mem::swap(&mut cells, &mut next_cells);
                }
            }
        }

        // }}}

        pub fn post_init(&mut self) {
            self.compute_distances();
        }

        // }}}
    }

    // {{{ Display

    impl fmt::Display for Map {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "nc:{} ", self.nc).unwrap();
            for idx in 0..self.cells.len() {
                write!(f, "{}: {} ", idx, self.cells[idx]).unwrap();
            }
            write!(f, "nb:{} ", self.nb).unwrap();
            write!(f, "nb:{} ", self.nb_allied).unwrap();
            write!(f, "nb:{} ", self.nb_opp).unwrap();
            Ok(())
        }
    }

    // }}}
    // }}}
    // {{{ Action

    #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
    pub enum Action {
        Wait,
        Beacon(Idx, Score),
        Line(Idx, Idx, Score),
        Message(&'static str),
    }

    impl Default for Action {
        fn default() -> Self {
            Action::Wait
        }
    }

    impl Action {
        pub fn from_string(input: &str) -> Self {
            let words : Vec<&str> = input.split(' ').collect();
            if words[0] == "WAIT" {
                cg_assert_eq!(words.len(), 1);
                Action::Wait
            } else
            if words[0] == "BEACON" {
                cg_assert_eq!(words.len(), 3);
                Action::Beacon(
                    words[1].parse::<Idx>().unwrap(),
                    words[2].parse::<Score>().unwrap())
            } else
            if words[0] == "LINE" {
                cg_assert_eq!(words.len(), 4);
                Action::Line(
                    words[1].parse::<Idx>().unwrap(),
                    words[2].parse::<Idx>().unwrap(),
                    words[3].parse::<Score>().unwrap())
            } else
            if words[0] == "MESSAGE" {
                Action::Message("") // don't care here
            } else {
                panic!("invalide action {input}");
            }
        }

        pub fn build_safe_message(msg: String) -> Self {
            // let msg = msg.replace(" ", "_");
            let msg = msg.into_boxed_str();
            let msg = Box::leak(msg);
            Action::Message(msg)
        }
    }

    impl fmt::Display for Action {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            let s = match self {
                Action::Wait  => format!("WAIT"),
                Action::Beacon(idx, strength) => format!("BEACON {} {}", idx, strength),
                Action::Line(idx0, idx1, strength) => format!("LINE {} {} {}", idx0, idx1, strength),
                Action::Message(m) => format!("MESSAGE {}", m),
            };
            write!(f, "{}", s).unwrap();
            Ok(())
        }
    }

    // }}}
    // {{{ Actions

    pub type Actions = FixedArray::<Action, MAX_ACTIONS>;
    pub type ActionsHistory = FixedArray::<Actions, MAX_HISTORY>;

    // }}}
    // {{{ Player

    pub type Usizes = FixedArray::<usize, MAP_SIZE>;

    #[derive(Default, Eq, PartialEq, Debug, Clone, Copy)]
    pub struct Player {
        pub score : Score,
        pub ants : Usizes,
    }

    impl fmt::Display for Player {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "score:{}", self.score).unwrap();
            write!(f, "ants:{}", self.ants).unwrap();
            Ok(())
        }
    }

    // }}}
    // State

    // {{{ Struct

    #[derive(Default, Debug, Clone, Copy)]
    pub struct State {
        pub turn: Turn, // locally computed
        pub pla: [Player; 2],
        pub resources: Usizes,
    }

    // }}}
    impl State {
        // {{{ builders

        pub fn init(&mut self) {
        }

        pub fn post_init(&mut self, map : &Map) {
            self.resources = Usizes::with_len(map.nc);
            self.pla[0].ants = Usizes::with_len(map.nc);
            self.pla[1].ants = Usizes::with_len(map.nc);

            // mimic a fake first normal turn
            for idx in 0..(map.nc) {
                self.resources[idx] = map.cells[idx].initial_resources as usize;
                // TODO initial ants???
            }
        }

        pub fn from_string(&mut self, lines: &Vec<String>, map : &Map) {
            // format: numberOfCells lines: "resources myAnts oppAnts"
            let nc = map.nc;

            for idx in 0..nc {
                let line : Vec<&str> = lines[idx].split(' ').collect();
                cg_assert_eq!(line.len(), 3);
                self.resources[idx] = line[0].parse::<usize>().unwrap();
                self.pla[0].ants[idx] = line[1].parse::<usize>().unwrap();
                self.pla[1].ants[idx] = line[2].parse::<usize>().unwrap();
            }
            self.turn += 1;
        }

        // }}}
        // {{{ ancillary functions

        pub fn is_finished(&self, map : &Map) -> bool {
            self.turn == MAX_TURN + 1
            || std::cmp::max(self.pla[0].score, self.pla[1].score)
                > map.initial_resources
        }

        // did I win?
        pub fn win(&self) -> (bool, Score, Score) {
            if self.pla[0].score == self.pla[1].score {
                (true, // ???
                self.pla[0].score,
                self.pla[1].score)
            } else {
                (self.pla[0].score > self.pla[1].score,
                self.pla[0].score,
                self.pla[1].score)
            }
        }

        // }}}
        // {{{ play_turn_fast apply only one player action and compute estimated score
        // return new state, used for tree compute

        pub fn play_turn_fast(&self, _me_action: &Action, _map: &Map) -> Self
        {
            let res = *self;

            res
        }

        // }}}
        // {{{ play_turn apply just the 2 players actions, used by simulation

        pub fn play_turn(&self, _me_action: &Action, _opp_action: &Action, _map: &Map) -> Self
        {
            let res = *self;

            res
        }

        // }}}
        // {{{ compute_possible_actions

        pub fn compute_possible_actions(&mut self, _map: &Map) -> Cands
        {
            let mut res = Cands::default();

            // let (len0, len1) = (self.pla[0].possible_actions.len(), self.pla[1].possible_actions.len());
            res.append( Cand {
                me: 0 as Idx,
                opp: 0 as Idx,
                ..Default::default()
            });

            res
        }

        // }}}
        // {{{ evaluate

        pub fn evaluate(&self) -> [f32; 2]
        {
            // XXX the result would be compared to the final score for terminal leaves, so don't
            // mess up and diverge a lot from the score!

            let res = [self.pla[0].score as f32, self.pla[1].score as f32];

            res
        }

        // }}}
    }

    // {{{ Display

    impl fmt::Display for State {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "turn:{}", self.turn).unwrap();
            write!(f, " me: {}", self.pla[0]).unwrap();
            write!(f, "opp: {}", self.pla[1]).unwrap();
            write!(f, "resources: {}", self.resources).unwrap();
            Ok(())
        }
    }

    // }}}
    // State END
    // {{{ Policy

    #[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
    pub enum Policy {
        Echo,
        AlwaysWait,
        LineClosest,
        LinesSmart,

        FirstCand, // always best move given my heuristic
        Scan, // simulate always until the game end, and open node smoothly
    }

    impl Policy {
        const VALUES: [Self; 6] = [
            Self::Echo,
            Self::AlwaysWait,
            Self::LineClosest,
            Self::LinesSmart,

            Self::FirstCand,
            Self::Scan
        ];

        pub fn new(input: &String) -> Self {
            for v in Policy::VALUES.iter() {
                // XXX to_string use Display
                if *input == v.to_string() {
                    return *v;
                }
            }
            panic!("unkown policy: {}", input);
        }
    }

    impl fmt::Display for Policy {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            match self {
                Policy::Echo => write!(f, "Echo").unwrap(),
                Policy::AlwaysWait => write!(f, "AlwaysWait").unwrap(),
                Policy::LineClosest => write!(f, "LineClosest").unwrap(),
                Policy::LinesSmart => write!(f, "LinesSmart").unwrap(),

                Policy::FirstCand => write!(f, "FirstCand").unwrap(),
                Policy::Scan => write!(f, "Scan").unwrap(),
            };
            Ok(())
        }
    }

    // }}}
    // {{{ AccStat

    // I really should not went in this direction…

    #[derive(Default, Copy, Clone, Debug)]
    pub struct AccStat<T> {
        pub sum: T,
        pub n: u32,
    }

    impl<T> AccStat<T>
    where T: AddAssign
    {
        pub fn acc(&mut self, v: T) {
            self.sum += v;
            self.n += 1;
        }
        pub fn acc_weight(&mut self, v: T, n: u32) {
            self.sum += v;
            self.n += n;
        }
    }

    // rust and floating point /o\, requiring TryFrom<u32> won't fit here, so we instantiate
    // manually... so create a trait
    pub trait AccStatGet {
        fn get(self) -> f32;
    }

    impl AccStatGet for AccStat<u32>
    {
        fn get(self) -> f32 {
            if self.n > 0 {
                self.sum as f32 / (self.n as f32)
            } else {
                f32::MIN
            }
        }
    }

    impl AccStatGet for AccStat<usize>
    {
        fn get(self) -> f32 {
            if self.n > 0 {
                self.sum as f32 / (self.n as f32)
            } else {
                f32::MIN
            }
        }
    }

    impl AccStatGet for AccStat<f32>
    {
        fn get(self) -> f32 {
            if self.n > 0 {
                self.sum / (self.n as f32)
            } else {
                f32::MIN
            }
        }
    }

    impl<T> PartialEq for AccStat<T>
    where T: PartialEq
    {
        fn eq(&self, other: &Self) -> bool {
            self.sum.eq(&other.sum) && self.n.eq(&other.n)
        }
    }

    impl<T> Add for AccStat<T>
    where T: Add<Output = T>
    {
        type Output = Self;

        fn add(self, other: Self) -> Self {
            Self {
                sum: self.sum + other.sum,
                n: self.n + self.n,
            }
        }
    }

    impl<T> AddAssign for AccStat<T>
    where T: AddAssign
    {
        fn add_assign(&mut self, other: Self) {
            self.sum += other.sum;
            self.n += other.n;
        }
    }

    // and then the generic display in diamond!
    impl<T> Display for AccStat<T>
    where T: Copy,
          AccStat<T>: AccStatGet,
    {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            if self.n > 0 {
                write!(f, " {:9.3}[{:5}]", self.get(), self.n).unwrap();
            } else {
                write!(f, " XXXXXXXXX[{:5}]", self.n).unwrap();
            }
            Ok(())
        }
    }

    // }}}
    // {{{ AccStatMinMax

    #[derive(Default, Copy, Clone)]
    pub struct AccStatMinMax<T> {
        pub min: T,
        pub max: T,
        pub n: u32,
    }

    impl<T> AccStatMinMax<T>
    where T: Default + Copy + Clone + PartialEq + PartialOrd + Add + AddAssign
    {
        pub fn acc(&mut self, v: T) {
            if self.n == 0 {
                self.min = v;
                self.max = v;
            } else {
                if v < self.min { self.min = v; }
                if v > self.max { self.max = v; }
            }
            self.n += 1;
        }
    }

    impl<T> Add for AccStatMinMax<T>
    where T: Default + Copy + Clone + PartialEq + PartialOrd + Add<Output = T>
    {
        type Output = Self;

        fn add(self, other: Self) -> Self {
            Self {
                min: if self.min < other.min { self.min } else { other.min },
                max: if self.max > other.max { self.max } else { other.max },
                n: self.n + self.n,
            }
        }
    }

    impl<T> AddAssign for AccStatMinMax<T>
    where T: Default + Copy + Clone + PartialEq + PartialOrd + Add<Output = T>
    {
        fn add_assign(&mut self, other: Self) {
            if other.max > self.max {
                self.max = other.max;
            }
            if other.min < self.min {
                self.min = other.min;
            }
            self.n += other.n;
        }
    }

    impl<T> fmt::Display for AccStatMinMax<T>
    where T: Default + Copy + Clone + PartialEq + PartialOrd + Add + AddAssign + Display
    {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, " [m:{:6}/M:{:6}]", self.min, self.max).unwrap();
            Ok(())
        }
    }

    // }}}
    // {{{ BotStatTurn

    #[derive(Default, Copy, Clone)]
    pub struct BotStatTurn {
        // turn duration
        pub step_duration: AccStatMinMax<u128>,

        // simulated game per turn
        pub simulated: AccStat<usize>,

        // depth game per turn
        pub depth: AccStat<usize>,
        pub depth_mm: AccStatMinMax<usize>,
    }

    impl BotStatTurn {
        pub fn report_step(&self) -> String {
            format!("durations:{} simulated:{} depth:{}",
                self.step_duration,
                self.simulated,
                self.depth)
        }
        pub fn report_final(&self) -> String {
            format!("durations:{} simulated:{} depth:{}",
                self.step_duration,
                self.simulated,
                self.depth)

        }
    }

    impl AddAssign for BotStatTurn {
        fn add_assign(&mut self, other: Self) {
            self.step_duration += other.step_duration;
            self.simulated += other.simulated;
            self.depth += other.depth;
        }
    }

    impl fmt::Display for BotStatTurn {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "durations:{} simulated:{} depth:{} depth_mm:{}",
                self.step_duration,
                self.simulated,
                self.depth,
                self.depth_mm).unwrap();
            Ok(())
        }
    }

    // }}}
    // Bot

    // {{{ Struct

    pub struct Bot {
        pub policy: Policy,
        pub mine: bool,
        pub options: MyOptions,

        pub step_start_time: Option<Instant>,

        // pub gc: GridChar,
        // pub tracking: Box<BotsTracking>,

        pub stats: BotStatTurn,

        // scan function internal cached data
        nodes : Nodes,

        // dup_filter : HashSet<(u32, u32)>,
        estimated_scores : Vec<Score>,

        max_actions_len : usize,

        rng: rand::rngs::ThreadRng,

        myrng: Rng,
    }

    // }}}
    // {{{ Candidate/Child struct

    #[derive(Copy, Clone)]
    pub struct Cand {
        // TODO
        // state / me / opp…
        me: Idx, // index of actions array used for this node
        opp: Idx, // index of actions array used for this node

        // live data
        nid: usize, // child node for this action, 0 means uncomputed candidate, Cand::DONE means end
        // statistics for this node (and its subtrees), filled once played
        win: AccStat<u32>,
        values: [f32; 2],
    }

    impl Cand {
        // nodes
        pub const ROOT : usize = 0;
        pub const FREE : usize = 0;
        pub const DONE : usize = usize::MAX;
        pub const EVICT : usize = usize::MAX - 1;
        // cand
        pub const UNKNOWN : usize = usize::MAX;

        pub fn win_avg(&self) -> f32 {
            self.win.get()
        }
    }

    // {{{ standard stuff

    impl PartialEq for Cand {
        fn eq(&self, other: &Self) -> bool {
            self.me == other.me && self.opp == other.opp
        }
    }
    impl Eq for Cand {}

    impl Ord for Cand {
        fn cmp(&self, other: &Self) -> cmp::Ordering {
            self.win.get().partial_cmp(&other.win.get()).unwrap()
        }
    }
    impl PartialOrd for Cand {
        fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Default for Cand {
        fn default() -> Self {
            Self {
                nid: Self::FREE,
                me: 0,
                opp: 0,
                win: AccStat::default(),
                values: [f32::MIN; 2],
            }
        }
    }

    // }}}
    // {{{ Display

    impl fmt::Display for Cand {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            let nid = self.nid;
            let nid =
                if nid == Cand::FREE { format!("    FREE") } else
                if nid == Cand::DONE { format!("    DONE") } else
                if nid == Cand::ROOT { format!("    ROOT") } else
                if nid == Cand::EVICT { format!("   EVICT") } else
                { format!("{:8}", nid) };
            write!(f, "me:{:2} opp:{:2} nid:{} win:{:6} values:{}",
                self.me,
                self.opp,
                nid,
                self.win,
                itertools::join(self.values.iter().map(|x| format!("{:8.4}", x)), "/"),
                ).unwrap();
            Ok(())
        }
    }

    // }}}

    pub type Cands = FixedArray::<Cand, MAX_CANDS>;

    // }}}
    // {{{ Node

    #[derive(Eq, PartialEq, Debug, Clone, Copy)]
    pub enum TreeScanAction {
        Expand,
        Continue,
        Deeper,
        Exit,
    }

    #[derive(Default, Copy, Clone)]
    pub struct Node {
        parent: usize, // the indexes of parent in the previous nodes
        parent_cand_idx: usize, // offset inside parent's cands
        cands: Cands, // arrays of cands indexes
        best_cands: [usize; 2], // index in cands for bests cand
        final_cand: usize, // the final choice
        trials: AccStat<u32>, // number of played games for this node
        state: State, // the state of the world for this node
    }
    pub type Nodes = Vec<Node>;

    impl Node {
        // {{{ new

        pub fn new(parent: usize, parent_cand_idx: usize, mut state: State, map: &Map) -> Self {
            Self {
                parent,
                parent_cand_idx,
                cands: state.compute_possible_actions(map),
                best_cands: [Cand::UNKNOWN; 2],
                final_cand: Cand::UNKNOWN,
                trials: AccStat::default(),
                state,
            }
        }

        // }}}
//         // {{{ dump
//
//         pub fn dump_by_node(lvl: u8, nodes: &Nodes, nid: usize, parent: usize, indent : usize)
//         {
//             let node = &nodes[nid];
//             if node.cands.len() == 0 {
//                 logln!(lvl, "{:indent$}n:{} [p:{} cid:{}/{}]", "",
//                     nid, parent, node.parent_cand_idx, nodes[parent].cands.len(), indent=indent);
//                 return;
//             }
//
//             let indent = indent + 2;
//
//             for cid in 0..node.cands.len() {
//                 let cand_nid = node.cands[cid].nid;
//                 if cand_nid == Cand::FREE || nid == Cand::EVICT {
//                     continue;
//                 }
//                 let me_action = node.cands[cid].me;
//                 let me_action = node.state.pla[0].possible_actions[me_action as usize];
//                 let opp_action = node.cands[cid].opp;
//                 let opp_action = node.state.pla[1].possible_actions[opp_action as usize];
//                 logln!(lvl, "{:indent$}n:{} [p:{} cid:{}/{}] {}{}{} {:13}/{:13} {}", "", cand_nid, nid, cid, node.cands.len(),
//                      if cid == node.best_cands[0] { "M" } else { " " },
//                      if cid == node.best_cands[1] { "O" } else { " " },
//                      if cid == node.final_cand { "X" } else { " " },
//                      me_action, opp_action,
//                     node.cands[cid], indent=indent);
//                 if cand_nid != Cand::DONE {
//                     Self::dump_by_node(lvl, nodes, cand_nid, nid, indent);
//                 }
//             }
//         }
//
//         pub fn dump(nodes: &Nodes, nid: usize, cid: usize, indent : usize)
//         {
//             let node = nodes[nid];
//             logln!(70, "{:indent$}n:{} cid:{}: {}", "",
//                 nid, cid, node.cands[cid], indent=indent);
//
//             let nid = node.cands[cid].nid;
//             if nid != Cand::FREE && nid != Cand::DONE {
//                 let node = nodes[nid];
//
//                 for cid in 0..node.cands.len() {
//                     if node.cands[cid].nid != Cand::FREE {
//                         Self::dump(nodes, nid, cid, indent);
//                     }
//                 }
//             }
//         }
//
//         // }}}
//         // {{{ cand mgmt / values mgmt
//
//         pub fn in_bests(&self, cand: usize) -> bool {
//             cand == self.best_cands[0]
//             || cand == self.best_cands[1]
//             || cand == self.final_cand
//         }
//
//         pub fn best_child(&self, who: usize) -> &Cand {
//             let idxmax = self.cands.iter().enumerate()
//                 .fold((0usize, f32::MIN), |acc, x|
//                     if x.1.values[who] > acc.1 {
//                         (x.0, x.1.values[who])
//                     } else { acc });
//
//             &self.cands[idxmax.0]
//         }
//
//         // propagate new value up in the tree
//         // if at least one is update, we return true, to continue
//         // cid change to these values
//         pub fn update_bests(&mut self, cid: usize, values: &mut [f32; 2]) -> bool
//         {
//             // TODO be smarter
//             let values_final_before = if self.final_cand == Cand::UNKNOWN {
//                 [f32::MIN; 2] } else { self.cands[self.final_cand].values };
//
//             // update slot
//             self.cands[cid].values = *values;
//
//             // compute best for each player
//             let mut best_cands = [0; 2];
//             let mut best_cands_v = [f32::MIN; 2];
//             for i in 0..self.cands.len() {
//                 if self.cands[i].nid != Cand::FREE {
//                     for pi in 0..2 {
//                         if i == 0 || self.cands[i].values[pi] > best_cands_v[pi] {
//                             best_cands_v[pi] = self.cands[i].values[pi];
//                             best_cands[pi] = i;
//                         }
//                     }
//                 }
//             }
//             self.best_cands = best_cands;
//
//             // compute final choice
//             self.final_cand = Cand::UNKNOWN;
//
//             let (me, opp) = (self.best_cands[0], self.best_cands[1]);
//             let (me, opp) = (self.cands[me].me, self.cands[opp].opp);
//             for i in 0..self.cands.len() {
//                 if self.cands[i].me == me && self.cands[i].opp == opp {
//                     self.final_cand = i;
//                     break;
//                 }
//             }
//             if self.final_cand == Cand::UNKNOWN {
//                 // we did not compute the combination pair, take my choice!
//                 self.final_cand = self.best_cands[0];
//             }
//
//             // update loop values data
//             *values = self.cands[self.final_cand].values;
//
//             values[0] != values_final_before[0]
//             || values[1] != values_final_before[1]
//             // values[0].partial_cmp(&values_final_before[0]) != Ordering::Equal cmd::Ordering::;
//             // values.partial_cmp(&values_final_before) != cmd::Ordering::;
//
//             // continue backpropagation only if we updated the final slot
//             // cid == self.final_cand
//         }
//
//         pub fn sanity_checks(nodes: &Nodes, nid: usize)
//         {
//             let node = &nodes[nid];
//             // not optimal, but we recompute the values
//
//             let expanded = node.cands.iter().filter(|x| x.nid != Cand::FREE).count();
//             if expanded > 0 {
//                 cg_assert!(node.best_cands[0] != usize::MAX);
//                 cg_assert!(node.best_cands[1] != usize::MAX);
//
//                 for pi in 0..2 {
//                     let best_value = node.cands[node.best_cands[pi]].values[pi];
//                     for cid in 0..node.cands.len() {
//                         cg_assert!(node.cands[cid].values[pi] <= best_value,
//                             "inconsistent values");
//                     }
//                 }
//                 cg_assert_eq!(node.cands[node.final_cand].me,
//                     node.cands[node.best_cands[0]].me, "best choice is not mine");
//             }
//         }
//
//         // }}}
//         // {{{ bfs
//
//         // TODO heap
//         // bfs for n days
//         pub fn bfs(nodes: &mut Nodes, parent : usize, days: u8, map: &Map)
//         {
//             for cid in 0..nodes[parent].cands.len() {
//                 if nodes[parent].cands[cid].nid == Cand::FREE {
//                     Node::expand_node(nodes, parent, cid, map);
//                     if nodes[parent].cands[cid].new_day {
//                         if days == 0 {
//                             continue;
//                         } else {
//                             Self::bfs(nodes, nodes[parent].cands[cid].nid, days - 1, map);
//                         }
//                     } else {
//                         Self::bfs(nodes, nodes[parent].cands[cid].nid, days, map);
//                     }
//                 }
//             }
//         }
//
//         // }}}
//         // {{{ dfs
//
//         // XXX do not generalize with other method to ensure tail call optimization \o/
//         pub fn dfs(nodes: &mut Nodes, parent : usize, map: &Map) -> bool
//         {
//             for child_idx in 0..nodes[parent].cands.len() {
//                 if nodes[parent].cands[child_idx].nid == Cand::FREE {
//                     let me = nodes[parent].cands[child_idx].me;
//                     let opp = nodes[parent].cands[child_idx].opp;
//                     let state = nodes[parent].state.play_turn(
//                         &nodes[parent].state.pla[0].possible_actions[me as usize],
//                         &nodes[parent].state.pla[1].possible_actions[opp as usize],
//                         map);
//
//                     if state.is_finished() {
//                         let (win, _, _) = state.win();
//                         nodes[parent].cands[child_idx].nid = Cand::DONE;
//                         let mut node_idx = parent;
//                         let mut node_child_idx = child_idx;
//                         loop {
//                             nodes[node_idx].cands[node_child_idx].win.acc(win as u32);
//                             if node_idx == Cand::ROOT {
//                                 return true;
//                             }
//                             cg_assert!(node_idx != nodes[node_idx].parent, "infinite loop");
//                             // XXX Achtung XXX order of statements
//                             node_child_idx = nodes[node_idx].parent_cand_idx;
//                             node_idx = nodes[node_idx].parent;
//                         }
//                     } else {
//                         let nid = nodes.len();
//                         nodes[parent].cands[child_idx].nid = nid;
//                         nodes.push(Node::new(parent, child_idx, state, map));
//                         logln!(99, "new(parent:{}, child_idx:{}/{}) with id {}",
//                             parent, child_idx, nodes[parent].cands.len(), nid);
//                         cg_assert_eq!(nodes[nid].parent, parent);
//                         cg_assert_eq!(nodes[nid].parent_cand_idx, child_idx);
//
//                         return Self::dfs(nodes, nid, map);
//                     }
//                 }
//             }
//             return false;
//         }
//
//         // }}}
//         // {{{ BFS with read only lambda
//
//         pub fn for_each<F>(nodes: &Nodes, parent : usize, depth: usize, f: &F)
//         where F: Fn(&Nodes, usize),
//         {
//             let parent = &nodes[parent];
//
//             for child in parent.cands.iter() {
//                 let nid = child.nid;
//                 if nid == Cand::FREE || nid == Cand::EVICT || nid == Cand::DONE {
//                     continue;
//                 }
//                 f(nodes, child.nid);
//                 Self::for_each(nodes, child.nid, depth + 1, f);
//             }
//         }
//
//         // }}}
//         // {{{ expand_node
//
//         pub fn expand_node(nodes: &mut Nodes, parent : usize, cid: usize, map: &Map)
//         {
//             // {{{ play turn + insert if not finished
//
//             if nodes[parent].cands[cid].nid != Cand::FREE {
//                 return
//             }
//
//             let me = nodes[parent].cands[cid].me;
//             let opp = nodes[parent].cands[cid].opp;
//             // XXX 'manual' placement new via &mut
//             let state = nodes[parent].state.play_turn(
//                 &nodes[parent].state.pla[0].possible_actions[me as usize],
//                 &nodes[parent].state.pla[1].possible_actions[opp as usize],
//                 map);
//
//             let finished = state.is_finished();
//             // evaluate only for new days
//             if state.day != nodes[parent].state.day {
//                 nodes[parent].cands[cid].new_day = true;
//                 nodes[parent].cands[cid].values = state.evaluate();
//             }
//
//             if finished {
//                 nodes[parent].cands[cid].win.acc(state.win().0 as u32);
//                 nodes[parent].cands[cid].nid = Cand::DONE;
//                 cg_assert!(nodes[parent].cands[cid].new_day);
//             } else {
//                 let nid = nodes.len();
//                 nodes[parent].cands[cid].nid = nid;
//                 nodes.push(Node::new(parent, cid, state, map));
//                 logln!(80, "create node {} from parent {} with cid {}, values:{}",
//                     nid, parent, cid,
//                     itertools::join(nodes[parent].cands[cid].values.iter().map(|x| format!("{:8.4}", x)), "/"));
//
//                 cg_assert_eq!(nodes[nid].parent, parent);
//                 cg_assert_eq!(nodes[nid].parent_cand_idx, cid);
//
//                 if nid == 7383 {
//                     logln!(0, "D:{} set values {}/{} to nid {}",
//                         state.day, nodes[parent].cands[cid].values[0],
//                         nodes[parent].cands[cid].values[1],
//                         nid);
//                 }
//             }
//
//             // }}}
//             // // {{{ propagate results upper
//
//             // let mut parent = parent;
//             // let mut cid = cid;
//             // loop {
//             //     if finished {
//             //         nodes[parent].cands[cid].win.acc(win as u32);
//             //     }
//             //     nodes[parent].trials.acc(1);
//             //     let updated = nodes[parent].update_bests(cid, &mut values);
//             //     // if !finished && !updated {
//             //     //     break; // no need to go upper
//             //     // }
//
//             //     if parent == Cand::ROOT {
//             //         break;
//             //     }
//
//             //     logln!(15, "up loop nid:{}->{} cid:{}->{} win:{} values:{} updated:{}",
//             //         parent, nodes[parent].parent,
//             //         cid, nodes[parent].parent_cand_idx,
//             //         win,
//             //         itertools::join(values.iter().map(|x| format!("{:8.4}", x)), "/"),
//             //         updated);
//
//             //     // XXX Achtung XXX order of statements
//             //     cid = nodes[parent].parent_cand_idx;
//             //     parent = nodes[parent].parent;
//             //     // TODO check if we can reput this optimization
//             //     // if !updated {
//             //     //     break;
//             //     // }
//             // }
//
//             // }}}
//         }
//
//         // }}}
//         // {{{ rollout
//
//         // result if number of final node on target day
//         pub fn rollout(nodes: &mut Nodes, parent : usize,
//             max_day : u8, n : f32,
//             rng: &mut Rng, map: &Map,
//             time_limit: &mut Instant, time_limit_v: u128)
//             -> (bool, usize) // timeout, number of expanded
//         {
//             // {{{ play already expanded node until the game's end
//             // take action 0 or final if defined
//             // we should stop just after a new day
//
//             let mut count = 0;
//
//             if parent == Cand::DONE || nodes[parent].state.day >= max_day {
//                 return (false, count);
//             }
//
//             let n = n / nodes[parent].cands.len() as f32;
//
//             // let mut entropy = 7727 + nodes.len();
//
//             for cid in 0..nodes[parent].cands.len() {
//                 if nodes.len() % 1000 == 0 && time_limit.elapsed().as_micros() > time_limit_v
//                 {
//                     return (true, count); // timeout
//                 }
//
//                 let nid = nodes[parent].cands[cid].nid;
//                 if nid == Cand::EVICT || nid == Cand::DONE {
//                     continue;
//                 }
//                 if nid == Cand::FREE {
//                     //
//                     //
//                     //
//                     // 1/n expand rule with entropy (first 3 nodes will be always expanded)
//                     // TODO: use our own statistics once computed
//                     // if cid < 3 + (rng.gen_u32() as usize) % nodes[parent].cands.len() {
//                     // if rng.gen_u32() % 10 > 8 {
//                     if true {
//                     // cheap entropy
//                     // entropy *= 7573;
//                     // if cid <= 3 + entropy % nodes[parent].cands.len() {
//                         Node::expand_node(nodes, parent, cid, map);
//                         count += 1;
//                         let nid = nodes[parent].cands[cid].nid;
//                         if nid == Cand::DONE { continue; }
//                         if nodes[parent].cands[cid].new_day {
//                             // new day played, check stats
//                             if false || n < 1. { // we've reached our exploration quota
//                                 return (false, count);
//                             }
//                         }
//                     } else {
//                         continue;
//                     }
//                 }
//                 let nid = nodes[parent].cands[cid].nid;
//
//                 // recurtion + propagate timeout
//                 let sub = Node::rollout(nodes, nid, max_day, n, rng, map, time_limit, time_limit_v);
//                 count += sub.1;
//                 if sub.0 {
//                     return (true, count); // timeout
//                 }
//             }
//
//             (false, count)
//
//             // }}}
//         }
//
//         // }}}
//         // {{{ compute_returns_value
//
//         // dedicated DFS walk to poperly compute values/returns
//         // returns win / values / evicted
//         pub fn compute_returns_values(nodes: &mut Nodes, parent : usize, target_day: u8)
//             -> (AccStat::<u32>, [f32;2], usize)
//         {
//             let mut win = AccStat::<u32>::default();
//             let mut best_values = [f32::MIN; 2];
//             let mut evicted = 0;
//
//             nodes[parent].best_cands = [0; 2]; // default is first choice
//
//             for cid in 0..nodes[parent].cands.len() {
//                 let nid = nodes[parent].cands[cid].nid;
//                 if nid == 0 || nid == Cand::DONE || nid == Cand::EVICT {
//                     continue;
//                 }
//                 if nodes[parent].state.day != nodes[nid].state.day {
//                     cg_assert!(nodes[parent].cands[cid].new_day);
//                     // dbg!(nodes[nid].state.day, target_day);
//                 }
//             }
//
//             for cid in 0..nodes[parent].cands.len() {
//                 let nid = nodes[parent].cands[cid].nid;
//                 if nid == Cand::FREE || nid == Cand::EVICT {
//                     continue;
//                 }
//                 // debug: force reset:
//                 // nodes[parent].cands[cid].win = AccStat::default();
//                 // nodes[parent].cands[cid].values = [f32::MIN; 2];
//
//                 // let value = nodes[parent].cands[cid].values[0];
//                 // let returns = value - parent_value;
//                 // let mut weight = 1;
//
//                 assert!(nodes[parent].state.day != target_day, "score recurstion problem");
//
//                 if (nodes[parent].cands[cid].new_day
//                     && nodes[parent].state.day == target_day - 1)
//                     || nid == Cand::DONE
//                 {
//                     assert_eq!(nodes[nid].state.day, target_day);
//                     // this is our target goal, we use these current values, and don't go
//                     // deeper
//                     let pseudo_win = nodes[parent].cands[cid].values[0] >=
//                         nodes[parent].cands[cid].values[1];
//                     let pseudo_win = pseudo_win as u32;
//                     nodes[parent].cands[cid].win.acc(pseudo_win);
//
//                     // be subtile…
//                     logln!(90, "final simulation node values: {}/{}",
//                         nodes[parent].cands[cid].values[0], nodes[parent].cands[cid].values[1]);
//                 } else {
//                     // logln!(0, "{} vs {}", nodes[parent].state.day, target_day);
//                     assert!(nodes[nid].state.day < target_day);
//                     // future values is on new days only
//                     let res = Self::compute_returns_values(nodes, nid, target_day);
//                     // win
//                     nodes[parent].cands[cid].win = res.0;
//                     nodes[parent].cands[cid].values = res.1;
//                     // // values: evaluation can only goes up like the score!
//                     // for i in 0..2 {
//                     //     if res.1[i] > nodes[parent].cands[cid].values[i] {
//                     //         nodes[parent].cands[cid].values[i] = res.1[i];
//                     //     }
//                     // }
//                     // evicted: accumulate during recursion
//                     evicted += res.2;
//                 }
//                 if nid == 7383 {
//                     logln!(0, "D:{} Target:{} set values {}/{} to nid {}",
//                         nodes[nid].state.day, target_day,
//                         nodes[parent].cands[cid].values[0],
//                         nodes[parent].cands[cid].values[1],
//                         nid);
//                 }
//          win += nodes[parent].cands[cid].win;
//
//                 for pi in 0..2 {
//                     if best_values[pi] < nodes[parent].cands[cid].values[pi] {
//                         best_values[pi] = nodes[parent].cands[cid].values[pi];
//                         nodes[parent].best_cands[pi] = cid;
//                     }
//                 }
//             }
//             cg_assert!(nodes[parent].state.day < target_day);
//
//             // compute final choice
//             nodes[parent].final_cand = Cand::UNKNOWN;
//             if nodes[parent].best_cands[0] != Cand::UNKNOWN && nodes[parent].best_cands[1] != Cand::UNKNOWN {
//                 let (me, opp) = (nodes[parent].best_cands[0], nodes[parent].best_cands[1]);
//                 let (me, opp) = (nodes[parent].cands[me].me, nodes[parent].cands[opp].opp);
//                 for i in 0..nodes[parent].cands.len() {
//                     let nid = nodes[parent].cands[i].nid;
//                     if nid == Cand::FREE || nid == Cand::EVICT { continue; }
//                     if nodes[parent].cands[i].me == me && nodes[parent].cands[i].opp == opp {
//                         nodes[parent].final_cand = i;
//                         break;
//                     }
//                 }
//             }
//             if nodes[parent].final_cand == Cand::UNKNOWN {
//                 // we did not compute the combination pair, take my choice!
//                 nodes[parent].final_cand = nodes[parent].best_cands[0];
//             }
//             // XXX test 'only me in the game'
//             nodes[parent].final_cand = nodes[parent].best_cands[0];
//
//             // // evict first days
//             // const EVICT_WINDOW : u8 = 30; // 5 days before target
//             // const EVICT_BAND : f32 = 1.;
//
//             // if nodes[parent].state.day + EVICT_WINDOW < target_day {
//             //     // let mut childs = SortedFixedArray::<usize, f32, MAX_CANDS>::default();
//
//             //     let mut thresholds = nodes[parent].cands[nodes[parent].final_cand].values;
//             //     thresholds[0] -= EVICT_BAND;
//             //     thresholds[1] -= EVICT_BAND;
//
//             //     for cid in 0..nodes[parent].cands.len() {
//             //         let nid = nodes[parent].cands[cid].nid;
//             //         if nid == 0 || nid == Cand::DONE || nid == Cand::EVICT {
//             //             // do not evict un-expanded tree
//             //             continue;
//             //         }
//             //         if nodes[parent].cands[cid].values[0] < thresholds[0]
//             //             && nodes[parent].cands[cid].values[1] < thresholds[1]
//             //         {
//             //             logln!(40, "evict {} with parent {}: {} vs {} and {} vs {}",
//             //                 nid, parent,
//             //                 nodes[parent].cands[0].values[0], thresholds[0],
//             //                 nodes[parent].cands[1].values[1], thresholds[1]);
//             //             nodes[parent].cands[cid].nid = Cand::EVICT;
//             //             evicted += 1;
//             //         }
//             //     }
//             // }
//             // if evicted > 0 {
//             //     logln!(0, "evicted {} nodes for day {}",
//             //         evicted, nodes[parent].state.day);
//             // }
//
//             (win, nodes[parent].cands[nodes[parent].final_cand].values, evicted)
//         }
//
//         // dedicated DFS walk to poperly compute values/returns
//         pub fn compute_returns_win(nodes: &mut Nodes, parent : usize) -> AccStat::<u32> // , [f32;2])
//         {
//             let mut win = AccStat::<u32>::default();
//             // let mut values = [f32::MIN; 2];
//             // let current_score = nodes[parent].state.win();
//
//             nodes[parent].best_cands = [Cand::UNKNOWN; 2];
//             let mut best_win = [0., 1.];
//
//             for cid in 0..nodes[parent].cands.len() {
//                 let nid = nodes[parent].cands[cid].nid;
//                 if nid == 0 {
//                     continue;
//                 }
//                 // let value = nodes[parent].cands[cid].values[0];
//                 // let returns = value - parent_value;
//                 // let mut weight = 1;
//
//                 // not game over, go deeper
//                 if nid != Cand::DONE {
//                     nodes[parent].cands[cid].win = Self::compute_returns_win(nodes, nid);
//                     // if !nodes[parent].cands[cid].new_day {
//                     //     // get back the future evaluation
//                     //     nodes[parent].cands[cid].values = res.1;
//                     // }
//                     // nodes[parent].cands[cid].account_returns(sub_returns);
//                     // ponderate by the node expand ratio
//                     // weight = nodes[parent].cands[cid].subtree_returns.n;
//                 }
//                 win += nodes[parent].cands[cid].win;
//
//                 let cand_win = nodes[parent].cands[cid].win.get();
//                 if cand_win > best_win[0] {
//                     best_win[0] = cand_win;
//                     nodes[parent].best_cands[0] = cid;
//                 }
//                 if cand_win < best_win[1] {
//                     best_win[1] = cand_win;
//                     nodes[parent].best_cands[1] = cid;
//                 }
//
//                 // subtree_returns.0 += returns * weight as f32;
//                 // subtree_returns.1 += weight;
//             }
//
//             // compute final choice
//             nodes[parent].final_cand = Cand::UNKNOWN;
//             if nodes[parent].best_cands[0] != Cand::UNKNOWN && nodes[parent].best_cands[1] != Cand::UNKNOWN {
//                 let (me, opp) = (nodes[parent].best_cands[0], nodes[parent].best_cands[1]);
//                 let (me, opp) = (nodes[parent].cands[me].me, nodes[parent].cands[opp].opp);
//                 for i in 0..nodes[parent].cands.len() {
//                     if nodes[parent].cands[i].me == me && nodes[parent].cands[i].opp == opp {
//                         nodes[parent].final_cand = i;
//                         break;
//                     }
//                 }
//             }
//             if nodes[parent].final_cand == Cand::UNKNOWN {
//                 // we did not compute the combination pair, take my choice!
//                 nodes[parent].final_cand = nodes[parent].best_cands[0];
//             }
//
//             win
//         }
//
//         // }}}
//         // {{{ get_best_sequence
//
//         // dedicated DFS walk to poperly compute values/returns
//         // returns win / values / evicted
//         pub fn get_best_action_sequence(nodes: &Nodes, parent : usize)
//             -> Vec<String>
//         {
//             let mut res = Vec::new();
//
//             let mut parent = parent;
//             loop {
//                 let best_idx = nodes[parent].best_cands[0];
//                 if best_idx == Cand::DONE {
//                     return res;
//                 }
//                 let best = &nodes[parent].cands[best_idx];
//                 let _values = &nodes[parent].cands[best_idx].values;
//                 let action = &nodes[parent].state.pla[0].possible_actions[best.me as usize];
//                 // res.push(format!("[{}]{}({}/{})", best_idx, *action, values[0], values[1]));
//                 res.push(format!("[{}]{}({:6.4}:{:9.4})", best_idx, *action,
//                 best.win.get(), best.values[0]));
//
//                 parent = best.nid;
//                 if parent == Cand::EVICT || parent == Cand::FREE || parent == Cand::DONE {
//                     return res;
//                 }
//             }
//         }
//
//         // }}}
//         // {{{ mixed expand
//
//         // BFS with choice on 1st level
//         // user function input: depth, choice index, choice lenght, node, cand
//         // user function decide to expand a candidate or not (then DFS)
//         // cb returns:
//         // - TreeScanAction::Expand => go deeper
//         // - TreeScanAction::Continue => BFS continue
//         // - TreeScanAction::Exit => full exit until root node
//
//         pub fn mixed_expand<F>(nodes: &mut Nodes, parent : usize,
//             win_day : u8,
//             depth: usize, f: &mut F, map: &Map) -> TreeScanAction
//         where F: FnMut(usize, usize, usize, &Node, &Cand) -> TreeScanAction, //  + Copy,
//         {
//             for cid in 0..nodes[parent].cands.len() {
//                 logln!(80, "mixed_expand(parent:{}, depth:{}, cid:{}/{}",
//                     parent, depth, cid, nodes[parent].cands.len());
//
//                 if nodes[parent].cands[cid].nid == Cand::DONE {
//                     continue; // already played
//                 }
//                 // user choice
//                 match f(depth, cid, nodes[parent].cands.len(),
//                      &nodes[parent], &nodes[parent].cands[cid])
//                 {
//                     // logic is simple: exit should be direct!
//                     TreeScanAction::Expand => {
//                         cg_assert_eq!(nodes[parent].cands[cid].nid, Cand::FREE);
//
//                         // {{{ play turn + insert if not finished
//
//                         Self::expand_node(nodes, parent, cid, map);
//
//                         // }}}
//                     },
//                     TreeScanAction::Deeper => {
//                         assert!(nodes[parent].cands[cid].nid != Cand::FREE, "deeper in a empty node");
//                         if Self::mixed_expand(nodes, nodes[parent].cands[cid].nid, win_day,
//                             depth + 1, f, map) == TreeScanAction::Exit
//                         {
//                             return TreeScanAction::Exit;
//                         }
//                     },
//                     TreeScanAction::Continue => {},
//                     TreeScanAction::Exit => {
//                         return TreeScanAction::Exit;
//                     },
//                 }
//             }
//             // the main loop is a BFS one
//             return TreeScanAction::Continue;
//         }
//
//         // }}}
    }

    // {{{ Display
    impl fmt::Display for Node {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            writeln!(f, "parent:{} parent_ci:{} best_cands:{}[/{}] final:{}",
                self.parent, self.parent_cand_idx,
                itertools::join(self.best_cands.iter(), "&"),
                self.cands.len(),
                self.final_cand,
                ).unwrap();
            for cid in 0..self.cands.len() {
                // skip unplayed
                if self.cands[cid].nid == 0 { continue; }
                writeln!(f, "{}{}{} {}/{}: {}",
                    if cid == self.best_cands[0] { "M" } else { " " },
                    if cid == self.best_cands[1] { "O" } else { " " },
                    if cid == self.final_cand { "X" } else { " " },
                    cid, self.cands.len(), self.cands[cid]).unwrap();
            }
            // writeln!(f, "state: {}", self.state).unwrap();
            // graph
            Ok(())
        }
    }

    // }}}
    // }}}

    impl Bot {
        // {{{ new

        pub fn new(options: &MyOptions, mine: bool, policy: Policy) -> Box<Self> {
            let nodes = if policy == Policy::Scan {
                Vec::with_capacity(NODES_POOL_SZ)
            } else {
                Vec::new()
            };

            Box::new(
                Self {
                    policy,
                    mine,
                    options: options.clone(),

                    step_start_time: Some(Instant::now()),

                    stats: BotStatTurn::default(),

                    nodes,

                    // dup_filter : HashSet::with_capacity(NODES_POOL_SZ),
                    estimated_scores : Vec::with_capacity(NODES_POOL_SZ),

                    max_actions_len : 0,

                    rng: rand::thread_rng(),

                    myrng: Rng::new(),
                })
        }

        // }}}
        // {{{ init / clear

        // call only once at game start (after parsing in codingame)
        // we have then 1s to build our internal stuff
        pub fn init<R: rand::Rng>(&mut self, _game: &Game, rng: &mut R) {

            let _now = Instant::now();

            if self.options.randomize && self.mine {
                self.options.shuffle(rng);
            }
            // graph
            // self.graph.nodes = grid.build_nodes();
            // self.graph.build_nodes_by_pos();
            // self.graph.build_distance(&game.distmaps);

            // I want trigger plenty of SEGFAULT!
            // codingame saturate really quick, so I can only book ¼
            // if self.policy == Policy::Scan {
            //     loop {
            //         for _ in 0..(1 << 10) {
            //             self.nodes.push(Node::default());
            //         }
            //         if game.g.start_time.elapsed().as_micros() > MAX_STARTUP_DURATION {
            //             break;
            //         }
            //     }
            //     self.clear();
            // }

            // build neighbors
            // self.gc.build(grid);

            // stat
            self.stats = BotStatTurn::default();
            logln!(80, "bot init: {}µs", _now.elapsed().as_micros());
        }

        // time after printin action
        pub fn clear(&mut self) {
            // root node
            self.nodes.clear();
            // self.dup_filter.clear();
            self.estimated_scores.clear();

            // logln!(0, "{}", self.stats);
            if !FORCE_ECHO {
                logln!(5, "{}", self.stats);
            }
        }

        // }}}
        // {{{ echo

        fn echo(&mut self, _game: &Game) -> (Actions, String)
        {
            let mut actions = Actions::default();
            actions.append(Action::Wait);
            (actions, String::from(""))
        }

        // }}}
        // {{{ always_wait

        fn always_wait(&mut self, _game: &Game) -> (Actions, String)
        {
            let mut actions = Actions::default();
            actions.append(Action::Wait);
            (actions, String::from(""))
        }

        // }}}
        // {{{ line_closest

        fn line_closest(&mut self, game: &Game) -> (Actions, String)
        {
            let mut actions = Actions::default();
            let my_base = game.map.nb_allied as Idx;

            // for d in 0..3 {
            //     eprintln!("at {}: {}", d, itertools::join(
            //         (0..(game.map.nc))
            //         .filter(|x| game.map.distance(my_base, *x) == d)
            //         .map(|x| format!("{}", x)),
            //         ","));
            // }

            match (0..(game.map.nc))
                .filter_map(|x| {
                    let r = game.state.resources[x];
                    if r > 0 {
                        let d = game.map.distance(my_base, x);
                        Some((x, r, d))
                    } else { None }
                })
                .min_by(|a, b| a.2.cmp(&b.2))
            {
                Some((idx, r, dist)) => {
                    actions.append(Action::Line(my_base, idx as Idx, 1));
                    (actions, format!("{}@{} at {}", r, my_base, dist))
                },
                None => {
                    actions.append(Action::Wait);
                    (actions, format!("no ressource to target"))
                },
            }
        }

        // }}}
        // {{{ lines_smart

        fn lines_smart(&mut self, game: &Game) -> (Actions, String)
        {
            let mut actions = Actions::default();
            let my_base = game.map.nb_allied as Idx;

            #[derive(Debug, Copy, Clone, PartialEq)]
            struct Cand {
                resources: usize,
                typ: CellType,
                distance: Idx,
                idx: Idx,
                turn: Turn,
            }
            impl Cand {
                pub fn heuristic(&self) -> f32 {
                    let resources = if self.typ == CellType::Egg {
                        std::cmp::min(10, self.resources * 10)
                        * if self.turn < 10 { 9 } else { 3 }
                    } else {
                        self.resources
                    };
                    (resources as f32) / (3. + self.distance as f32)
                }
            }
            impl PartialOrd for Cand {
                fn partial_cmp(&self, other: &Cand) -> Option<cmp::Ordering> {
                    other.heuristic().partial_cmp(&self.heuristic())
                }
            }

            let turn = game.turn;
            let mut cands : Vec<_> = (0..(game.map.nc))
                .filter_map(|idx| {
                    let resources = game.state.resources[idx];
                    if resources > 0 {
                        let distance = game.map.distance(my_base, idx);
                        let typ = game.map.cells[idx].typ;
                        let idx = idx as Idx;
                        Some( Cand { resources, typ, distance, idx, turn })
                    } else { None }
                })
                .collect();
            cands.sort_by(|a, b| a.partial_cmp(b).unwrap());
            for (rank, target) in cands.iter().take(5).enumerate() {
                dbg!(&target);
                actions.append(
                    Action::Line(my_base, target.idx, match rank {
                        0 => 2,
                        1 => 1,
                        2 => 1,
                        3 => 1,
                        4 => 1,
                        5 => 1,
                        6 => 1,
                        _ => panic!(),
                    }));
            }
            (actions, format!("{} cands", cands.len()))
        }

        // }}}
        // {{{ first_cand

        fn first_cand(&mut self, _game: &Game) -> (Actions, String)
        {
            let mut actions = Actions::default();
            actions.append(Action::Wait);
            (actions, String::from(""))
        }

        // }}}
        // {{{ scan

        fn scan(&mut self, _game: &Game) -> (Actions, String)
        {
            // let mut time_count = self.step_start_time.unwrap();

            // self.nodes.push(Node::new(Cand::ROOT, 0, game.state.clone(), &game.map));
            // // realign score to avoid potential overflow at 255
            // // self.nodes[Cand::ROOT].state.realign_scores();
            // let _head_value = self.nodes[Cand::ROOT].state.evaluate();
            // let max_depth_len;

            // // {{{ MCTS

            // // // build the tree
            // // const MAX_BEAM_DEPTH : usize = MAX_CANDS; // for the moment *all* candidates pairs
            // // const MAX_ROLLOUT_STEP : usize = 1_000;
            // // let mut head = Cand::ROOT;
            // // let mut cands = SortedFixedArray::<usize, f32, MAX_BEAM_DEPTH>::default();
            // // let mut mcts_loop = 0;

            // // 'mcts: loop {
            // //     mcts_loop += 1;
            // //     // // expand all child in case of there were still some missing
            // //     // // we sort them bu decreasing heuristics
            // //     // for cid in 0..self.nodes[head].cands.len() {
            // //     //     Node::expand_node(&mut self.nodes, head, cid, &game.g);
            // //     //     let cand = &self.nodes[head].cands[cid];
            // //     //     if cand.nid != Cand::DONE {
            // //     //         cands.append(cand.nid, cand.values[0] + cand.values[1]);
            // //     //     }
            // //     // }
            // //     // logln!(10, "expansion:{} in {} nodes",
            // //     //     itertools::join(cands.iter().map(|x| format!("{}", x)), ","),
            // //     //     self.nodes.len());
            // //     // if cands.len() == 0 {
            // //     //     logln!(0, "hummm");
            // //     //     break 'mcts;
            // //     // }
            // //     // simulation
            // //     let mut simulation_loop = 0;
            // //     'simulation: loop {
            // //         let mut expanded = 0;
            // //         let max_day = MAX_TURN + 10; // game.state.day + 10; // for max_day in [3, 5, 7, 10, 15].iter() {
            // //         // for selected in cands.iter() {
            // //         //    expanded += Node::rollout(&mut self.nodes, *selected, max_day, &mut self.rng, &game.g);
            // //             expanded += Node::rollout(&mut self.nodes, head, max_day, &mut self.rng, &game.g);
            // //             simulation_loop += 1;
            // //         // }
            // //         if expanded == 0 || simulation_loop > MAX_ROLLOUT_STEP {
            // //             break 'simulation;
            // //         }
            // //     }
            // //     // backpropagation
            // //     Node::compute_returns(&mut self.nodes, head);

            // //     if time_count.elapsed().as_micros() > MAX_TURN_DURATION {
            // //         break 'mcts;
            // //     }
            // //     // selection
            // //     // let choice = self.nodes[head].final_cand;
            // //     // let choice = self.nodes[head].best_cands[0]; // me for the moment
            // //     // logln!(0, "head {}->{}[win:{}/{}] : {}",
            // //     //     head, self.nodes[head].cands[choice].nid,
            // //     //     self.nodes[head].cands[choice].win.sum, self.nodes[head].cands[choice].win.n,
            // //     //     self.nodes[head].state.pla[0].possible_actions[self.nodes[head].cands[choice].me as usize]);
            // //     // head = self.nodes[head].cands[choice].nid;
            // //     if head == Cand::DONE || head == Cand::FREE {
            // //         break;
            // //     }
            // //     // we need at least 100 games before expand
            // //     // if simulated > 100 {
            // //     // }
            // // }
            // // logln!(0, "rollouts done in {} loops: nodes.len():{} head trials:{} head:{}",
            // // mcts_loop,
            // // self.nodes.len(), self.nodes[head].trials.sum, self.nodes[head]);
            // // build the tree
            // // }}}
            // // {{{ DFS incrementaux en jours
            // // const MAX_BEAM_DEPTH : usize = MAX_CANDS;
            // // const MAX_EXPAND_STEP : usize = 1_000;
            // // let mut cands = FixedArray::<usize, MAX_BEAM_DEPTH>::default();
            // // cands.append(Cand::ROOT);
            // // let mut days = 1;

            // // loop {
            // //     // BFS
            // //     for cand in cands.iter() {
            // //         Node::bfs(&mut self.nodes, *cand, 1, &game.g);
            // //     }
            // //     // let mut expand_count = 0;
            // //     // loop {
            // //     //     let expanded = Node::rollout(&mut self.nodes, head, max_day, &mut self.rng, &game.g);
            // //     //     expand_count += 1;
            // //     //     if expanded == 0 || expand_count > MAX_EXPAND_STEP {
            // //     //         break;
            // //     //     }
            // //     // }
            // //     // backpropagation, always from root?
            // //     let target_day = game.state.day + days;
            // //     Node::compute_returns_values(&mut self.nodes, Cand::ROOT, target_day);

            // //     if time_count.elapsed().as_micros() > MAX_TURN_DURATION {
            // //         break;
            // //     }
            // //     // selection
            // //     // let choice = self.nodes[head].final_cand;
            // //     // let choice = self.nodes[head].best_cands[0]; // me for the moment
            // //     // logln!(0, "head {}->{}[win:{}/{}] : {}",
            // //     //     head, self.nodes[head].cands[choice].nid,
            // //     //     self.nodes[head].cands[choice].win.sum, self.nodes[head].cands[choice].win.n,
            // //     //     self.nodes[head].state.pla[0].possible_actions[self.nodes[head].cands[choice].me as usize]);
            // //     // head = self.nodes[head].cands[choice].nid;
            // //     if head == Cand::DONE || head == Cand::FREE {
            // //         break;
            // //     }
            // //     // we need at least 100 games before expand
            // //     // if simulated > 100 {
            // //     // }
            // //     break;
            // // }
            // // logln!(0, "BFS done in {} loops: nodes.len():{} head trials:{} head:{}",
            // // 1,
            // // self.nodes.len(), self.nodes[head].trials.sum, self.nodes[head]);
            // // }}}

            // const ROLLOUT_STEP : usize = 1000;
            // let head = Cand::ROOT;
            // let mut days = 1;
            // let mut simulation_loop = 0;
            // let mut evicted = 0;

            // loop {
            //     let target_day = game.state.turn + days;

            //     // expand up to ROLLOUT_STEP new final day 'target_day' nodes
            //     let (timeout, expanded) = Node::rollout(&mut self.nodes, head,
            //         target_day, ROLLOUT_STEP as f32,
            //         &mut self.myrng, &game.g,
            //         &mut time_count, MAX_TURN_DURATION);

            //     if timeout { // timeout means that latest statistics are potentially not correct
            //         break;
            //     }

            //     // backpropagation, always from root
            //     let (_, _, new_evicted) = Node::compute_returns_values(&mut self.nodes, Cand::ROOT, target_day);

            //     if target_day == 4 { break; }
            //     if target_day == MAX_TURN {
            //         break;
            //     }
            //     // if target_day > 2 {
            //     //     break;
            //     // }
            //     simulation_loop += 1;
            //     days += 1;
            //     evicted += new_evicted;
            //     logln!(3, "rollout done in {} loops ({}) for {} days, expanded:{}, nodes.len():{}, head trials:{}, new_evicted:{}, head:{}, final:{}",
            //         simulation_loop, time_count.elapsed().as_micros(),
            //         days,
            //         expanded,
            //         self.nodes.len(),
            //         self.nodes[head].trials.sum,
            //         new_evicted,
            //         self.nodes[head],
            //         self.nodes[head].cands[self.nodes[head].final_cand]);
            //     if days > MAX_TURN - 1 {
            //         break;
            //     }
            // }
            // logln!(1, "rollout done in {} loops for {} days, nodes.len():{}, head trials:{}, evicted:{}, head:{} final:{}",
            // simulation_loop, days,
            // self.nodes.len(), self.nodes[head].trials.sum, evicted, self.nodes[head],
            // self.nodes[head].cands[self.nodes[head].final_cand]);
            // max_depth_len = days as usize;

            // // {{{ old method with callback // build the tree
            // // let (mut call, mut max_depth_len) = (0, 1);
            // // let (mut explore_depth) = (0);
            // // let (mut exploit_depth) = (0);
            // // let (mut _expand) = (0);
            // // let win_day = game.state.day + 10; // MAX_TURN
            // // loop {
            // //     if Node::mixed_expand(&mut self.nodes, Cand::ROOT, win_day, 0usize,
            // //     &mut |depth, choice, choice_len, node, cand| {
            // //         // if depth > 3 { return TreeScanAction::Continue; }

            // //         logln!(20, "lambda f(depth:{}, choice:{}, choice_len:{}, cand:{}), call:{}",
            // //             depth, choice, choice_len, cand, call);

            // //         call += 1;
            // //         if depth + 1 > max_depth_len { max_depth_len = depth + 1; }
            // //         if call % 64 == 0 && time_count.elapsed().as_micros() > MAX_TURN_DURATION {
            // //             return TreeScanAction::Exit;
            // //         }

            // //         const EXPLORATION_N : usize = 10000;
            // //         // exploration phase: BFS on EXPLORATION_N nodes
            // //         if call < EXPLORATION_N {
            // //             // full expand of *all* nodes until depth of N then only firsts
            // //             if cand.nid == Cand::FREE {
            // //                 explore_depth = cmp::max(explore_depth, depth);
            // //                 return TreeScanAction::Expand;
            // //             }
            // //             // if depth <= explore_depth + 1 {
            // //                 return TreeScanAction::Deeper;
            // //             // }
            // //             // if cand.nid == Cand::FREE && (depth < explore_depthEXPLORATION_DEPTH || choice < 4) {
            // //             //     return TreeScanAction::Expand;
            // //             // }
            // //             // // explore best candidates only once we'rve reached correct stats
            // //             // if cand.nid != Cand::FREE && cand.trials.n < 4 {
            // //             //     return TreeScanAction::Deeper;
            // //             // }
            // //             // // select only best ones
            // //             // if cand.nid != Cand::FREE && node.in_bests(choice) {
            // //             //     return TreeScanAction::Deeper;
            // //             // }
            // //             // return TreeScanAction::Continue;
            // //         }
            // //         // exploitation phase: expand only best cands
            // //         if cand.nid != Cand::FREE {
            // //             // expand good nodes
            // //             if node.final_cand != Cand::UNKNOWN {
            // //                 if cand.values[0] > node.cands[node.final_cand].values[0] - 4.
            // //                 || cand.values[0] > node.cands[node.final_cand].values[0] - 4.
            // //                 {
            // //                     return TreeScanAction::Deeper;
            // //                 } else {
            // //                     return TreeScanAction::Continue;
            // //                 }
            // //             }
            // //             return TreeScanAction::Deeper;
            // //         }
            // //         // do not expand free node where we have already chose
            // //         // if cand.trials.n > 4 && node.in_bests(choice) {
            // //         if node.final_cand != Cand::UNKNOWN
            // //             && node.cands[node.final_cand].trials.n > 4
            // //         {
            // //                 return TreeScanAction::Continue;
            // //         }
            // //         exploit_depth = cmp::max(exploit_depth, depth);
            // //         return TreeScanAction::Expand;
            // //         // return TreeScanAction::Exit;
            // //         // {{{ previous target code
/*
            // //         // exploration phase
            // //         const EXPLORATION_N : usize = 1000;
            // //         if expand < EXPLORATION_N {
            // //             let choice_openess = choice as f32 / choice_len as f32; // [0;1[
            // //             let exploration_cursor = expand as f32 / EXPLORATION_N as f32; // [0;1[
            // //             const EXPLORATION_OVERSHOOT : f32 = 2.;
            // //             let openess = choice_openess * exploration_cursor * EXPLORATION_OVERSHOOT;
            // //             let depth_ratio = (depth + 1) as f32 / max_depth_len as f32; // [epsilon;1]
            // //             return if openess < depth_ratio {
            // //                 expand += 1;
            // //                 TreeScanAction::Expand
            // //             } else {
            // //                 TreeScanAction::Continue
            // //             };
            // //         } else {
            // //             // exploitation phase

            // //             // first explore should be fast
            // //             if depth < 3000 {
            // //                 TreeScanAction::Expand
            // //             } else {
            // //                 TreeScanAction::Continue
            // //             }
            // //         }
            // //         // if n_game == 0 {
            // //         //     return choice < 1;
            // //         // }
            // //         // // always expand at least once the first candidate
            // //         // if stat.n == 0 && choice < choice_len / 3 + 1  {
            // //         //     return true;
            // //         // } else {
            // //         //     return false;
            // //         // }
*/
            // //         // }}}
            // //     }, &game.g) == TreeScanAction::Exit {
            // //         break;
            // //     }
            // // };
            // // logln!(0, "call:{} explore_depth:{} exploit_depth:{}", call, explore_depth, exploit_depth);
            // // }}}

            // // {{{ printout tree in a "pretty" way

            // if true {
            //     // 1st level
            //     let lvl = 30;
            //     logln!(lvl, "root:{}", &self.nodes[Cand::ROOT]);
            //     for cid in 0..self.nodes[Cand::ROOT].cands.len() {
            //         let nid = self.nodes[Cand::ROOT].cands[cid].nid;
            //         if nid != Cand::FREE {
            //             logln!(lvl, " 1st:{}", &self.nodes[nid]);
            //             // Node::dump(&self.nodes, nid, cid, 2+cid);
            //         }
            //     }

            //     // full tree in bfs mode
            //     let lvl = 10;
            //     logln!(lvl, "dump_by_node:{}", &self.nodes[Cand::ROOT]);
            //     Node::dump_by_node(lvl, &self.nodes, Cand::ROOT, 0, 0);

            //     // Node::for_each(&self.nodes, Cand::ROOT, 0usize, &Node::sanity_checks);
            // }

            // // }}}

            // // logln!(0, "head: {}", self.nodes[0]);
            // logln!(1, "duration: {}µs",
            //     self.step_start_time.unwrap().elapsed().as_micros());

            // // let (action, cand) = (Action::Wait, Cand::default()); // self.nodes[0].best_child();
            // let best_idx = self.nodes[Cand::ROOT].best_cands[0];
            // if best_idx == Cand::DONE { // gni?
            //     return (Action::Wait, String::from("invalid best cand!"));
            // }
            // let best = &self.nodes[Cand::ROOT].cands[best_idx];
            // let action = self.nodes[Cand::ROOT].state.pla[0].possible_actions[best.me as usize];
            // logln!(2, "best {} with {}", action, best);

            // let from_state = &game.state;
            // logln!(40, "from {}\n{}", from_state,
            //     from_state.pla[0].trees.dump_pretty(&from_state.pla[1].trees, &game.g));

            // // last move is "different"
            // if best.nid != Cand::DONE {
            //     let to_state = &self.nodes[best.nid].state;
            //     logln!(40, "to [idx:{}] {}\n{}", best.nid, to_state,
            //         to_state.pla[0].trees.dump_pretty(&to_state.pla[1].trees, &game.g));
            // }

            // let msg = format!("{:6} / V:{:9.3}/{:9.3} / D:{:3} N:{:8} / {}",
            //     self.step_start_time.unwrap().elapsed().as_micros(),
            //     best.values[0], best.values[1],
            //     max_depth_len,
            //     self.nodes.len(),
            //     itertools::join(
            //         Node::get_best_action_sequence(&self.nodes, Cand::ROOT).iter(),
            //         ", ")
            //     );
            // // let msg = format!("V:{:9.3}/{:9.3} {}",
            // //     best.values[0], best.values[1],
            // //     itertools::join(
            // //         Node::get_best_action_sequence(&self.nodes, Cand::ROOT).iter(),
            // //         ", ")
            // //     );

            // // let action = Action::Wait; // reproductible behavior for bench

            // self.stats.simulated.acc(self.nodes.len());
            // self.stats.depth.acc(max_depth_len);
            // self.stats.depth_mm.acc(max_depth_len);

            // (action, msg)

            let mut actions = Actions::default();
            actions.append(Action::Wait);
            (actions, String::from(""))
        }

        // }}}
        // {{{ turn

        pub fn step_stat(&mut self) {
            let duration = self.step_start_time.unwrap().elapsed().as_micros();
            self.step_start_time = None;

            self.stats.step_duration.acc(duration);
        }

        pub fn next_actions(&mut self, game: &Game) -> (Actions, String) {
            logln!(90, "next_actions: state:{}", game.state);
            logln!(90, "next_actions: bot:{}", self);

            let (action, msg) = match self.policy {
                Policy::Echo => self.echo(game),
                Policy::AlwaysWait => self.always_wait(game),
                Policy::LineClosest => self.line_closest(game),
                Policy::LinesSmart => self.lines_smart(game),

                Policy::FirstCand => self.first_cand(game),
                Policy::Scan => self.scan(game),
            };
            logln!(20, "choice: mine:{} action: {}", self.mine, action);
            self.step_stat();
            if self.mine { // only report "my" bot
                logln!(20, "step: {}", self.stats.report_step());
            }

            (action, msg)
        }

        // }}}
    }

    // {{{ Display

    impl fmt::Display for Bot {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            writeln!(f, "policy: {}", self.policy).unwrap();
            writeln!(f, "mine: {}", self.mine).unwrap();
            writeln!(f, "options: {}", self.options).unwrap();
            // graph
            Ok(())
        }
    }

    // }}}
    // Bot END
    // {{{ Simulation

    #[derive(Debug, Clone, Copy)]
    pub struct Simulation {
        // game characterisics
        map: Map,
        state: State,
    }

    impl Simulation {
        pub fn new<R: rand::Rng>(_rng: &mut R) -> Self {
            todo!();
        }

        // generate a game from a simulation
        pub fn feed_state(&self) -> State {
            self.state.clone()
        }

        // apply after player actions, for cleanup, bonus, ...
        pub fn complete_state(&mut self, state: &mut State) {
            // see code in FallChallenge2020
            self.update_state(state);
        }

        // function here for logic reason, but do not use self
        pub fn update_state(&self, _state: &mut State) {
        }
    }

    // }}}
    // {{{ Game

    pub struct Game {
        pub map: Map,
        pub start_time : Instant,
        pub state: State,
        pub turn: Turn, // memorized turn
        pub actions: ActionsHistory, // real action history
        pub opp_actions: ActionsHistory, // real action history
        pub options: MyOptions,
        // not reseted after a game
        pub win: bool,
        pub simulation: Option<Simulation>,
    }

    impl Game {
        // {{{ new/init functions

        pub fn new(options: &MyOptions) -> Box<Self> {
            Box::new(Self {
                map: Map::default(),
                start_time: Instant::now(),
                state: State::default(),
                turn: 0,
                actions: ActionsHistory::default(),
                opp_actions: ActionsHistory::default(),
                options: options.clone(),
                win: false,
                simulation: None,
            })
        }

        pub fn init(&mut self, simulation: Option<&Simulation>) -> Instant {
            let now = Instant::now();

            self.turn = 0;
            self.win = false;
            self.actions.clear();
            self.opp_actions.clear();

            if self.options.simulation || self.options.learn {
                assert!(simulation.is_some());
                let simulation = simulation.unwrap();
                self.state = simulation.feed_state();
                // self.map = Map::new(simulation.forest, now);
                // XXX avoid this copy
                self.simulation = Some(simulation.clone());
            }

            now
        }

        // called after map loading
        pub fn post_init(&mut self, _simulation: Option<&Simulation>) {
            self.map.post_init();
            self.state.post_init(&self.map);
        }

        // }}}

        pub fn flip_players(&mut self) {
            self.state.pla.swap(0, 1);
            std::mem::swap(&mut self.actions, &mut self.opp_actions);
        }

        // {{{ parse inputs

        // parse initial game data
        pub fn initial_load_from_input(&mut self) -> Vec<String> {
            logln!(80, "initial_load_from_input");
            let mut lines = Vec::new();
            let dump_line = self.options.verbose >= 50;

            // cells: 'N' value then N lines
            lines.push(get_stdin_line(dump_line));
            // take the time just after the first parsed line
            self.start_time = Instant::now();

            let n = lines.last().unwrap().parse::<usize>().unwrap();
            for _ in 0..n {
                lines.push(get_stdin_line(dump_line));
            }
            // 3 numberOfBases lines
            for _ in 0..3 {
                lines.push(get_stdin_line(dump_line));
            }

            for _line in &lines {
                logln!(99, "parsed:{}", _line);
            }

            lines
        }

        // parse turn data and return timestamp
        pub fn load_from_input(&mut self) -> (Instant, Vec<String>) {
            logln!(80, "load_from_input");
            let mut lines = Vec::new();

            let dump_line = self.options.verbose >= 50;

            // 'self.nc' lines: resources myAnts oppAnts
            lines.push(get_stdin_line(dump_line));
            // take the time just after the first parsed line
            let now = Instant::now();
            for _ in 1..(self.map.nc) {
                lines.push(get_stdin_line(dump_line));
            }

            for _line in &lines {
                logln!(99, "parsed:{}", _line);
            }

            // build map
            self.state.from_string(&lines, &self.map);

            // detect me/opponent behavior from its score evolution
            logln!(60, "score:{}/{}",
                self.state.pla[0].score,
                self.state.pla[1].score);

            (now, lines)
        }

        // }}}
        // {{{ steps

        pub fn update_apply_step(&mut self) -> (Instant, Vec<String>) {
            let mut now = Instant::now();
            let mut lines = Vec::new();

            if !self.options.simulation && !self.options.learn {
                // incremental input
                let res = self.load_from_input();
                now = res.0;
                lines = res.1;
            }
            logln!(40, "update_applystep {}µs", now.elapsed().as_micros());

            (now, lines)
        }

        #[cfg(feature = "customizable")]
        pub fn simulate_one_turn(&mut self) {
            cg_assert!(self.simulation.is_some());
            cg_assert!(self.actions.len() > 0);
            let action = self.actions.last();
            cg_assert!(self.opp_actions.len() > 0);
            let opp_action = self.opp_actions.last();
            cg_assert!(self.simulation.is_some());
            self.state = self.state.play_turn(
                &action, &opp_action, &self.g); // , &mut self.simulation.as_mut().unwrap());
            logln!(70, "state after {} & {}: {}", action, opp_action, self.state);

        }

        pub fn print_actions(&self) {
            let action = self.actions.last();
            println!("{}", itertools::join(action.iter(), ";"));
            std::io::stdout().flush().unwrap();
        }

        pub fn end_turn(&mut self) {
            self.turn += 1;

            // end of game
            if self.state.is_finished(&self.map)
            {
                // remaining bonus ?
                self.win = self.state.pla[0].score > self.state.pla[1].score;

                logln!(5, "end game, {} vs. {} @{}",
                    self.state.pla[0].score,
                    self.state.pla[1].score,
                    self.turn);
            }
        }

        // }}}
    }

    impl fmt::Display for Game {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            writeln!(f, "state: {}", self.state).unwrap();
            if self.actions.len() > 0 {
                write!(f, "last action: {:?}",
                    self.actions.last()).unwrap();
            }
            if self.opp_actions.len() > 0 {
                write!(f, "last opp_action: {:?}",
                    self.opp_actions.last()).unwrap();
            }
            writeln!(f, "options: {}", self.options).unwrap();
            Ok(())
        }
    }

// }}}
}

// {{{ offlinecontroler

pub mod offline_controler {
    pub use crate::main::*;

    pub struct Controler {
        pub game: Box<Game>,
        pub bot: Box<Bot>,
        pub opp_bot: Option<Box<Bot>>,
        lines: Vec::<String>,
        game_counter: usize,
    }

    impl Controler {
        pub fn new(game: Box<Game>,
            bot: Option<Box<Bot>>,
            opp_bot: Option<Box<Bot>>) -> Self
        {
            Controler {
                game,
                bot: bot.unwrap(),
                opp_bot,
                lines: Vec::with_capacity(200),
                game_counter: 0,
            }
        }

        // XXX apply fn to both options, in game and bot
        // TODO: find a way to share it
        pub fn change_options<F>(&mut self, mut callback: F)
            where F: Copy + FnMut(&mut MyOptions)
        {
            callback(&mut self.game.options);
            callback(&mut self.bot.options);
            if self.opp_bot.is_some() {
                callback(&mut self.opp_bot.as_mut().unwrap().options);
            }
        }

        // {{{ offline simulation mode: against a local bot on simulated game(s)

        #[cfg(feature = "customizable")]
        pub fn event_loop_simulation(mut self) {
            #[derive(Default)]
            struct MultiStat {
                pub bot_stats: BotStatTurn,
                pub game_scores: [Score; 2],
                pub game_n: usize,
                pub game_win: usize,
            }
            impl Drop for MultiStat {
                fn drop(&mut self) {
                    let _n = self.game_n as f32;
                    logln!(1, "{0} avg score: {1} {2}, win rate: {3:3.0}%",
                        self.bot_stats.report_final(),
                        self.game_scores[0] as f32 / _n,
                        self.game_scores[1] as f32 / _n,
                        100. * self.game_win as f32 / _n);
                    std::io::stderr().flush().unwrap();
                }
            }
            let mut stats = MultiStat::default();

            assert!(self.game.options.simulation);
            let mut header_dumped = false;

            let mut rng = get_rng!(self.game.options.seed);
            let mut now = Instant::now();
            let simulation = Simulation::new(&mut rng);

            let (mut _duration_sum, mut _duration_n) = (0u128, 0);
            let (mut _score_sum, mut _score_n) = (0, 0);
            let mut need_init = true;
            loop {
                if need_init {
                    now = Instant::now();
                    self.game.init(Some(&simulation));
                    self.bot.init(&self.game, &mut rng);
                    if let Some(opp_bot) = &mut self.opp_bot {
                        opp_bot.init(&self.game, &mut rng);
                    }
                    logln!(60, "init: {}µs", self.game.start_time.elapsed().as_micros());
                    need_init = false;
                }

                // technically unrequired for policy Scan...
                self.game.state.compute_possible_actions(&self.game.g);

                self.bot.step_start_time = Some(Instant::now());
                logln!(50, "game for me: {}", self.game);
                let (action, _msg) = self.bot.next_actions(&self.game);
                self.game.actions.append(action);

                if let Some(opp_bot) = &mut self.opp_bot {
                    opp_bot.step_start_time = Some(Instant::now());
                    self.game.flip_players();
                    logln!(50, "game for opp: {}", self.game);
                    let (action, _msg) = opp_bot.next_actions(&self.game);
                    self.game.actions.append(action); // will be flipped with opp_actions
                    self.game.flip_players();
                }

                self.game.simulate_one_turn();

                // use opponent time for clean operations
                self.bot.clear();
                if let Some(opp_bot) = &mut self.opp_bot {
                    opp_bot.clear();
                }

                if self.game.state.is_finished(&self.map) {
                    self.game_counter += 1;
                    logln!(1, " bot {}", self.bot.stats.report_final());

                    let duration = now.elapsed().as_micros();
                    duration_sum += duration;
                    duration_n += 1;

                    let win = self.game.state.win();
                    game_win += win.0 as u32;
                    game_n += 1;


                    logstart!(1, "new game ({0:5}/{1:5}) in {2:8}µs",
                        self.game_counter,
                        self.game.options.game_n,
                        duration);
                    logcont!(1, " (avg:{0:8.0})",
                        duration_sum as f64 / duration_n as f64);
                    logcont!(1, " bot {}", self.bot.stats.report_final());
                    logcont!(1, " score me:{0:3} opp:{1:3}",
                        win.1, win.2);
                    logcont!(1, " avg success:{0:3.0}%",
                        100. * (game_win as f32) / (game_n as f32));
                    logstop!(1, "");

                    if self.game.options.game_n != -1
                    &&  self.game_counter as isize > self.game.options.game_n {
                        return;
                    }
                    if get_ctrlc_stop() {
                        return;
                    }
                    // respawn a game
                    need_init = true;

                    // output csv
                    if !header_dumped {
                        self.bot.options.dump_parameters_names("");
                        print!(",");
                        if let Some(opp_bot) = &mut self.opp_bot {
                            opp_bot.options.dump_parameters_names("_opp");
                        }
                        println!(",score,score_opp,win");
                        header_dumped = true;
                    }
                    self.bot.options.dump_parameters();
                    print!(",");
                    if let Some(opp_bot) = &mut self.opp_bot {
                        opp_bot.options.dump_parameters();
                    }
                    println!(",{},{},{}",
                        self.game.state.pla[0].score,
                        self.game.state.pla[1].score,
                        self.game.win);

                    if self.game.options.game_n != -1
                    &&  self.game_counter as isize > self.game.options.game_n {
                        return;
                    }
                    if get_ctrlc_stop() {
                        return;
                    }
                    need_init = true;

                }
            }
        }

        // }}}
        // {{{ offline learn mode

        pub fn event_loop_learn(mut self) {
            assert!(self.game.options.learn);

            let mut rng = get_rng!(self.game.options.seed);
            let mut now = Instant::now();
            let simulation = Simulation::new(&mut rng);

            let (mut duration_sum, mut duration_n) = (0u128, 0);
            let (mut game_win, mut game_n) = (0, 0);
            let mut need_init = true;
            loop {
                if need_init {
                    now = Instant::now();
                    self.game.init(Some(&simulation));
                    self.bot.init(&self.game, &mut rng);
                    if let Some(opp_bot) = &mut self.opp_bot {
                        opp_bot.init(&self.game, &mut rng);
                    }
                }
                self.bot.step_start_time = Some(now);
                logln!(50, "game for me: {}", self.game);
                let (action, _msg) = self.bot.next_actions(&self.game);
                self.game.actions.append(action);

                if let Some(opp_bot) = &mut self.opp_bot {
                    opp_bot.step_start_time = Some(now);
                    let (action, _msg) = opp_bot.next_actions(&self.game);
                    self.game.opp_actions.append(action);
                }

                self.game.update_apply_step();
                self.game.end_turn();
                self.bot.clear(); // use opponent time for clean operations
                if let Some(opp_bot) = &mut self.opp_bot {
                    opp_bot.clear();
                }
                if self.game.state.is_finished(&self.game.map) {
                    self.game_counter += 1;

                    let duration = now.elapsed().as_micros();
                    duration_sum += duration;
                    duration_n += 1;

                    let win = self.game.state.win();
                    game_win += win.0 as u32;
                    game_n += 1;


                    logstart!(1, "new game ({0:5}/{1:5}) in {2:8}µs",
                        self.game_counter,
                        self.game.options.game_n,
                        duration);
                    logcont!(1, " (avg:{0:8.0})",
                        duration_sum as f64 / duration_n as f64);
                    logcont!(1, " bot {}", self.bot.stats.report_final());
                    logcont!(1, " score me:{0:3} opp:{1:3}",
                        win.1, win.2);
                    logcont!(1, " avg success:{0:3.0}%",
                        100. * (game_win as f32) / (game_n as f32));
                    logstop!(1, "");

                    if self.game.options.game_n != -1
                    &&  self.game_counter as isize > self.game.options.game_n {
                        return;
                    }
                    if get_ctrlc_stop() {
                        return;
                    }
                    // respawn a game
                    need_init = true;
                }
            }
        }

        // }}}
        // {{{ event_loop

        pub fn event_loop(mut self) {

            // {{{ offline learn mode

            if self.game.options.learn {
                return self.event_loop_learn();
            }

            // }}}
            // {{{ offline "codingame" mode: play against another bot on simulation

            #[cfg(feature = "customizable")]
            if self.game.options.simulation {
                return self.event_loop_simulation();
            }

            // }}}

            let mut rng = get_rng!(self.game.options.seed);
            if FORCE_ECHO || self.bot.policy == Policy::Echo {
                set_log_lvl(0);
                self.game.options.verbose = 0;
            }

            self.game.init(None);
            let lines = self.game.initial_load_from_input();
            if FORCE_ECHO || self.bot.policy == Policy::Echo {
                for line in lines.iter() {
                    eprintln!("{}", line);
                }
                std::io::stderr().flush().unwrap();
            }
            self.game.map.from_string(&lines);
            self.game.post_init(None);

            self.bot.init(&self.game, &mut rng);
            if !FORCE_ECHO {
                eprintln!("init: {}µs", self.game.start_time.elapsed().as_micros());
            }

            // main codingame "live" loop
            loop {
                let (time, lines) = self.game.update_apply_step();
                self.bot.step_start_time = Some(time);
                if self.game.state.is_finished(&self.game.map) {
                    break;
                }

                let (mut action, msg) = self.bot.next_actions(&self.game);
                // live debug messages (XXX leak messages)
                if action.len() == 0 { action.append(Action::Wait); };
                action.append(Action::build_safe_message(msg));
                self.game.actions.append(action);
                self.game.print_actions();
                self.bot.clear(); // use opponent time for clean operations

                if FORCE_ECHO || self.bot.policy == Policy::Echo {
                    for line in lines.iter() {
                        eprintln!("{}", line);
                    }
                    std::io::stderr().flush().unwrap();
                }

                // stats.bot_stats += self.bot.stats;

                self.game.turn += 1;
            }
        }

        // }}}
    }
}

// }}}
// {{{ event_loop

fn launch_game(options: &main::MyOptions) {
    use crate::main::*;
    use crate::offline_controler::Controler;

    cg_assert!(options.bot);

    let game = Game::new(options);

    let bot = if options.bot {
        Some(Bot::new(options, true, options.bot_policy))
    } else {
        None
    };
    let opp_bot = if options.bot && (options.simulation || options.learn) {
        Some(Bot::new(options, false, options.opp_policy))
    } else {
        None
    };

    Controler::new(game, bot, opp_bot).event_loop();
}

// }}}
// {{{ main

// #[cfg(test)]
// mod annex;
// #[cfg(test)]
// pub use crate::annex::tests;

fn main() {

    // // basic one
    // assert_eq!(std::mem::size_of::<main::Tree>(), 4, "Tree");
    // assert_eq!(std::mem::size_of::<main::FCell>(), 8, "FCell");
    // assert_eq!(std::mem::size_of::<main::Action>(), 3, "Action");
    // assert_eq!(std::mem::size_of::<main::Actions>(), 256, "Actions"); // by design
    // assert_eq!(std::mem::size_of::<main::Trees>(), 8 * 5, "Trees");

    // // game one
    // assert_eq!(std::mem::size_of::<main::Player>(), 304, "Player");
    // assert_eq!(std::mem::size_of::<main::State>(), 616, "State");

    // simulation
    // 512 for cand
    // assert_eq!(std::mem::size_of::<main::Cand>(), 32, "Cand"); // by design
    // assert_eq!(std::mem::size_of::<main::Cands>(), 488, "Cands"); // by design
    // assert_eq!(std::mem::size_of::<main::Node>(), 1152, "Node");
    // 1k for cand
    // assert_eq!(std::mem::size_of::<main::Cand>(), 32, "Cand"); // by design
    // assert_eq!(std::mem::size_of::<main::Cands>(), 1000, "Cands"); // by design
    // assert_eq!(std::mem::size_of::<main::Node>(), 1664, "Node");
    // 2k for cands
    // assert_eq!(std::mem::size_of::<main::Cands>(), 1000, "Cands"); // by design
    // assert_eq!(std::mem::size_of::<main::Node>(), 2688, "Node");

    // cg_assert_eq!(std::mem::size_of::<main::NodeFast>(), 512);

    // cg_assert_eq!(std::mem::size_of::<main::Node>(), 26904);


    #[cfg(feature = "customizable")]
    ctrlc::set_handler(move || {
        // 2nd time we exit directly
        if main::get_ctrlc_stop() {
            std::process::exit(1);
        }
        main::ctrlc_stop();
    })
    .expect("Error setting Ctrl-C handler");

    let myoptions = main::MyOptions::new();

    launch_game(&myoptions);

    // TOTO tuning mode
}

// }}}
// vim: set ft=rust:
// vim: set tw=99:
