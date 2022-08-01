export function isInt (x) {
  return typeof(x) == "number" && ((x|0) === x);
};

export function getProperty (name) {
  return function (x) {
    return x[name];
  };
};
