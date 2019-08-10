var path = require('path');
var webpack = require('webpack')

module.exports = {
  entry: __dirname + '/src/index.js',

  output: {
    path: __dirname + '/dist',
    filename: 'main.js'
  },

  module: {
    rules: [
      {
        test: /\.wav$/,
        use: 'file-loader'
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
      },
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader'],
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          { loader: 'elm-hot-webpack-loader' },
          {
            loader: 'elm-webpack-loader',
            options: {
              optimize: false,
              cwd: __dirname,
              debug: false
            },
          }
        ],
      },
    ]
  },

  plugins: [
    new webpack.HotModuleReplacementPlugin()
  ],

  mode: 'development',

  devServer: {
    inline: true,
    hot: true,
    stats: 'errors-only',
    index: 'index.html',
    contentBase: "./dist",
  },

  resolve:{
    modules: [path.resolve(__dirname, 'src'), 'node_modules'],
    extensions: ['.js', '.elm', '.css']
  }
}
