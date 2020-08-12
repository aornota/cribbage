# ![cribbage](https://raw.githubusercontent.com/aornota/cribbage/master/src/ui/public/tpoc-32x32.png) | cribbage (_pre-α_)

_Work-in-progress towards a cribbage game..._

#### Development prerequisites

- [Microsoft .NET Core 3.1 SDK](https://dotnet.microsoft.com/download/dotnet-core/3.1/): I'm currently using 3.1.302
- [FAKE 5](https://fake.build/): _dotnet tool install fake-cli --global_; I'm currently using 5.20.2
- [Paket](https://fsprojects.github.io/Paket/): _dotnet tool install paket --global_; I'm currently using 5.249.0
- [Yarn](https://yarnpkg.com/lang/en/docs/install/): I'm currently using 1.22.4
- [Node.js (LTS)](https://nodejs.org/en/download/): I'm currently using 12.18.3

##### Also recommended

- [Microsoft Visual Studio Code](https://code.visualstudio.com/download/) with the following extensions:
    - [Microsoft C#](https://marketplace.visualstudio.com/items?itemName=ms-vscode.csharp)
    - [Ionide-fsharp](https://marketplace.visualstudio.com/items?itemName=ionide.ionide-fsharp)
    - [EditorConfig for VS Code](https://marketplace.visualstudio.com/items?itemName=editorconfig.editorconfig)
    - [Rainbow Brackets](https://marketplace.visualstudio.com/items?itemName=2gua.rainbow-brackets)
- [Google Chrome](https://www.google.com/chrome/) with the following extensions:
    - [React Developer Tools](https://chrome.google.com/webstore/detail/react-developer-tools/fmkadmapgofadopljbjfkapdkoienihi/)
    - [Redux DevTools](https://chrome.google.com/webstore/detail/redux-devtools/lmhkpmbekcpmknklioeibfkpmmfibljd/)
- ([Microsoft .NET Framework 4.7.2 Developer Pack](https://dotnet.microsoft.com/download/dotnet-framework/net472/): this appeared to resolve problems with Intellisense in
_[build.fsx](https://github.com/aornota/gibet/blob/master/build.fsx)_)

#### Running / building

Before first run:

- _paket install_

Build targets:

- Run/watch for development (Debug) without (re)building web workers: _fake build -t run_
- Run/watch for development (Debug) including (re)building web workers: _fake build -t run-with-build-ui-workers_
- Build for production (Release): or _fake build -t build_
- Publish to gh-pages (Release): _fake build -t publish-gh-pages_
- Run the dev console (Debug): _fake build -t run-dev-console_ (or just _fake build_)
- Run the tests (Release): _fake build --t run-tests_
- Help (lists useful build targets): _fake build -t help_
