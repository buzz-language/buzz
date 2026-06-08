# Buzz package manager

- Manifest tells buzz how to fetch the package
- Package are downloaded into `vendors/`
- Package should always have a `src` directory (so i guess we remove `rootDir`) from which import path for it are resolved
- `pkg:mypackage/some/dir/source.buzz` resolves then to `venors/mypackage/src/some/dir/source.buzz`
- buzz std lib is resolved with `buzz:<std lib>`
- To not make the current package something special we could just make a symlink into vendor?
- [!] don't forget to not @embedFile all buzz std lib. Actuall we should only @embed for wasm and read files directly from BUZZ_PATH/lib/*.buzz on other builds

The real question is: do we assume we run a buzz package/program from its package dir or from anywhere?

-> either we're in the project directory
-> or we're running from something like /usr/share/buzz/ which contains /vendors etc.

=> We must distinguish between cwd and root director from which we resolve buzz scripts

!! call without parentheses unexpected with dot calls
