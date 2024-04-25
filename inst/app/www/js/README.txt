To use validate.js in the client's browser, it has to be compiled from validate.js.src with browserify.

npm install -g ajv // the validation library
npm install ajv-formats --save // from R package's root folder, i. e. the one containing DESCRIPTION etc.
npm install -g browserify
$ browserify validate.src.js -o validate.js
