import Foundation
import ArgumentParser

struct buzz: ParsableCommand {
    @Argument() var file: String
    
    func run() throws {
        let url = URL(fileURLWithPath: file)
        let data = try Data(contentsOf: url)
        
        _ = Scanner(source: String(decoding: data, as: UTF8.self)).scan()
    }
}
