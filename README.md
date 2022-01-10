# Unicode graphs

[Live here: unicode-graphs.netlify.app](https://unicode-graphs.netlify.app/)

A static website to edit graphs as unicode characters. Graphs can be shared via the url at any time.

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher
* [node.js](https://nodejs.org) with [npm](https://www.npmjs.com/)
* An F# editor like Visual Studio, Visual Studio Code with [Ionide](http://ionide.io/) or [JetBrains Rider](https://www.jetbrains.com/rider/).

## Deployment

A github action builds the Fable project on push and deploys it to netlify

## Building and running the app

* Install JS dependencies: `yarn`
* Install F# dependencies: `yarn start`
* After the first compilation is finished, in your browser open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.

## Project structure

### npm

JS dependencies are declared in `package.json`, while `package-lock.json` is a lock file automatically generated.

### Webpack

[Webpack](https://webpack.js.org) is a JS bundler with extensions, like a static dev server that enables hot reloading on code changes. Fable interacts with Webpack through the `fable-loader`. Configuration for Webpack is defined in the `webpack.config.js` file. Note this sample only includes basic Webpack configuration for development mode, if you want to see a more comprehensive configuration check the [Fable webpack-config-template](https://github.com/fable-compiler/webpack-config-template/blob/master/webpack.config.js).

### F#

The sample contains F# files: the project (.fsproj) and all source file (.fs) in the `src` folder.

### Web assets

Images and other assets like an icon can be found in the `public` folder.
