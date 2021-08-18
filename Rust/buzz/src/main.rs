extern crate buzz;
fn main() {
    let source = std::fs::read_to_string("address.txt");

    if source.is_ok() {
        let scanner = buzz::scanner::scanner_from_string(&source.unwrap());
    }
}
