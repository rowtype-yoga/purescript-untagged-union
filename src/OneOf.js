exports.jsTypeOf = function (x) {
  return typeof(x);
};

exports.isInt = function (x) {
  return typeof(x) == "number" && ((x|0) === x);
};
