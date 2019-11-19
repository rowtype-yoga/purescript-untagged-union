exports.isInt = function (x) {
  return typeof(x) == "number" && ((x|0) === x);
};

exports.getProperty = function (name) {
  return function (x) {
    return x[name];
  };
};
