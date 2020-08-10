const glob = require('fast-glob');
const path = require('path');

const outputDir = path.join(__dirname, '../public/Workers')
const workerGlobs = ['*.fs']

console.log('Bundling workers for production...');

const flatten = arr => {
    return arr.reduce(function (flat, toFlatten) {
        return flat.concat(Array.isArray(toFlatten) ? flatten(toFlatten) : toFlatten);
    }, []);
}

const createExport = fileName => {
    const options =
        {
            entry: fileName,
            output: {
                path: outputDir,
                filename: path.basename(fileName).replace(/\.fs(x)?$/, '.js'),
                library: path.basename(fileName, path.extname(fileName)),
                libraryTarget: 'umd'
            },
            mode: 'production',
            devtool: '',
            resolve: {
                symlinks: false
            },
            module: {
                rules: [
                    {
                        test: /\.fs(x|proj)?$/,
                        use: {
                            loader: 'fable-loader',
                            options: {
                                allFiles: true,
                                define: []
                            }
                        }
                    },
                    {
                        test: /\.js$/,
                        exclude: /node_modules/,
                        use: {
                            loader: 'babel-loader',
                            options: {
                                presets: [
                                    ['@babel/preset-env', {
                                        modules: false,
                                        useBuiltIns: 'usage',
                                        corejs: 3
                                    }]
                                ]
                            }
                        }
                    }
                ]
            },
            target: 'webworker'
        }
    return options
}

module.exports = flatten(
    workerGlobs
        .map(pattern => path.join(__dirname, pattern).replace(/\\/g, '/'))
        .map(workerGlob => glob.sync(workerGlob).map(createExport))
)
