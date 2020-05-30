# ![cribbage](https://raw.githubusercontent.com/aornota/cribbage/master/src/resources/tpoc-32x32.png) | cribbage (pre-_α_)

_Work-in-progress towards a cribbage game..._

#### Development prerequisites

- [Microsoft .NET Core 3.1 SDK](https://dotnet.microsoft.com/download/dotnet-core/3.1/): I'm currently using 3.1.201
- [FAKE 5](https://fake.build/): _dotnet tool install fake-cli --global_; I'm currently using 5.19.1
- [Paket](https://fsprojects.github.io/Paket/): _dotnet tool install paket --global_; I'm currently using 5.243.0

##### Also recommended

- [Microsoft Visual Studio Code](https://code.visualstudio.com/download/) with the following extensions:
    - [Microsoft C#](https://marketplace.visualstudio.com/items?itemName=ms-vscode.csharp)
    - [Ionide-fsharp](https://marketplace.visualstudio.com/items?itemName=ionide.ionide-fsharp)
    - [EditorConfig for VS Code](https://marketplace.visualstudio.com/items?itemName=editorconfig.editorconfig)
    - [Rainbow Brackets](https://marketplace.visualstudio.com/items?itemName=2gua.rainbow-brackets)
- ([Microsoft .NET Framework 4.7.2 Developer Pack](https://dotnet.microsoft.com/download/dotnet-framework/net472/): this appeared to resolve problems with Intellisense in
_[build.fsx](https://github.com/aornota/gibet/blob/master/build.fsx)_)

#### Running / building

Before first run:

- _paket install_

Build targets:

- Run the heuristics console (Debug): _fake build -t run-heuristics-console_ (or just _fake build_)
- Run the tests (Release): _fake build --t run-tests_
- Help (lists key targets): _fake build -t help_
