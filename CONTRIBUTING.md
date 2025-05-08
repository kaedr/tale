# Contributing to TALE
Thank you for your interest in contributing to TALE. There are many ways to contribute to the TALE community. For all of them, we ask that you review existing [Issues](https://github.com/kaedr/tale/issues) to see if someone is already talking about it.

## Types of [Issues](https://github.com/kaedr/tale/issues)

### [Documentation](https://github.com/kaedr/tale/issues?q=is%3Aissue%20state%3Aopen%20label%3Adocumentation)
Have a suggestion for how to improve the documentation? Please submit an issue describing in as much detail as possible what you feel is lacking and suggestions for how best to improve it.

### [Bugs](https://github.com/kaedr/tale/issues?q=is%3Aissue%20state%3Aopen%20label%3Abug)
Found something in TALE that doesn't seem to be working quite right? Please submit a bug report using the appropriate Issue template and providing as much detail as possible about how to recreate it and what you expect to happen.

### [Enhancements](https://github.com/kaedr/tale/issues?q=is%3Aissue%20state%3Aopen%20label%3Aenhancement)
Did the good idea fairy visit you in the night? Considering submitting a Feature request for new functionality you think would help make TALE more awesome to use.

## [Pull Requests](https://github.com/kaedr/tale/pulls)
When submitting a pull request, a few things to keep in mind:
- It should tie to an open [Issue](https://github.com/kaedr/tale/issues) that's being addressed.
- It should not run afoul of the [License](https://github.com/kaedr/tale/blob/main/LICENSE) or [Code of Conduct](https://github.com/kaedr/tale/blob/main/CODE_OF_CONDUCT.md).
- It must pass all [checks](https://github.com/kaedr/tale/blob/main/.github/workflows/rust.yml) before being merged.
    - `cargo check`
    - `cargo clippy`
    - `cargo fmt`
    - All Tests Passing
        - Linux
        - Windows
        - MacOS
- Any changes should be reflected in the appropriate documentation.
- It must have the required approvals (Currently just BDFL until there's enough traction to have a core team).
