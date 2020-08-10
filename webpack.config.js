var path = require('path');
var webpack = require('webpack');
var htmlWebpackPlugin = require('html-webpack-plugin');
var copyWebpackPlugin = require('copy-webpack-plugin');
var miniCssExtractPlugin = require('mini-css-extract-plugin');

var config = {
    indexHtmlTemplate: './src/ui/index.html'
    , fsharpEntry: './src/ui/ui.fsproj'
    , scssEntry: './src/ui/style/cribbage.scss'
    , outputDir: './src/ui/publish'
    , assetsDir: './src/ui/public'
    , devServerPort: 8080
    , babel: {
        presets: [
            ["@babel/preset-env", {
                modules: false
                , useBuiltIns: 'usage'
				, corejs: 3
            }]
        ]
    }
}

var isProduction = !process.argv.find(v => v.indexOf('webpack-dev-server') !== -1);
console.log('Bundling for ' + (isProduction ? 'production' : 'development') + '...');

// HtmlWebpackPlugin automatically injects <script> or <link> tags for generated bundles.
var commonPlugins = [
    new htmlWebpackPlugin({
        filename: 'index.html'
        , template: resolve(config.indexHtmlTemplate)
    })
];

module.exports = {
    entry: { app: [resolve(config.fsharpEntry), resolve(config.scssEntry)] }
    , output: {
        path: resolve(config.outputDir)
        , filename: isProduction ? '[name].[hash].js' : '[name].js'
    }
    , mode: isProduction ? 'production' : 'development'
    , devtool: isProduction ? '' : 'eval-source-map'
    , optimization: {
        splitChunks: {
            cacheGroups: {
                commons: {
                    test: /node_modules/
                    , name: 'vendors'
                    , chunks: 'all'
                }
            }
        }
    }
    , plugins: isProduction
        ? commonPlugins.concat([
            new copyWebpackPlugin({ patterns: [{ from: resolve(config.assetsDir) }]})
            , new miniCssExtractPlugin({ filename: 'style.[hash].css' })
        ])
        : commonPlugins.concat([
            new webpack.HotModuleReplacementPlugin()
            , new webpack.NamedModulesPlugin()
        ])
    , resolve: {
        symlinks: false
    }
    , devServer: {
        publicPath: "/"
        , contentBase: resolve(config.assetsDir)
        , port: config.devServerPort
        , hot: true
        , inline: true
    }
    , module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/
                , use: {
                    loader: 'fable-loader'
                    , options: {
                        babel: config.babel
                        , define: isProduction ? [ 'ADAPTIVE_NO_TYPE_TESTS' ] : [ 'ADAPTIVE_NO_TYPE_TESTS', 'DEBUG' ]
                    }
                }
            }
            , {
                test: /\.js$/
                , exclude: /node_modules/
                , use: {
                    loader: 'babel-loader'
                    , options: config.babel
                },
            }
            , {
                test: /\.(sass|scss|css)$/,
                use: [
                    isProduction ? miniCssExtractPlugin.loader : 'style-loader'
                    , 'css-loader'
                    , {
                        loader: 'resolve-url-loader',
                    }
                    , {
                        loader: 'sass-loader',
                    }
                ]
            }
            , {
                test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*)?$/
                , use: ['file-Loader']
            }
        ]
    }
};

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}
