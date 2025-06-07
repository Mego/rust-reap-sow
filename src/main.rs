use reap_sow_macros::reap;

fn main() {
    dbg!(reap!(sow(3) + 100));
    dbg!(reap!(sow(5) * sow(100)));
}
