# Tilia

Tilia is a new experimental formatter for Haskell source code. The primary
design choices of the project are the following:

* Use `ghc-lib-parser` for parsing, thus achieving correct parsing at all
  times.
* Let single vs multiline layout be influenced by the original input,
  similar to the principles found in some other formatters in the ecosystem.
* Admit no configuration.
* Ensure high-quality formatting of comments.
* Provide first-class support for CPP.
* Implement correct treatment of operator chains with absolute precision
  regarding the precedence and fixities of individual operators rather than
  relying on approximations or collection of data about fixities in the
  ecosystem in advance.

## License

Copyright © 2026–present Mark Karpov

Distributed under the BSD 3-clause license.
