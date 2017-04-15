var _elm_lang$core$Main$bad = function (arg) {
	return function (x) {
		return x;
	}(
		function () {
			var _p0 = arg;
			if (_p0.ctor === 'Just') {
				return true;
			} else {
				var _p1 = true;
				return _p1 ? false : _p1;
			}
		}());
};
var _elm_lang$core$Main$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Main$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
