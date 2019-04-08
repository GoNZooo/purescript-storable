exports.localStorage_ = function (just, nothing) {
  return localStorage ? just(localStorage) : nothing;
};

exports.sessionStorage_ = function (just, nothing) {
  return sessionStorage ? just(sessionStorage) : nothing;
};

exports.keys_ = function (storage) {
  return Object.keys(storage);
};