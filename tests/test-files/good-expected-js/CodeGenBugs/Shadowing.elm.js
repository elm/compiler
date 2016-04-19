var _elm_lang$core$Main$toName = function (a) {
	var name = function () {
		var _p0 = a;
		if (_p0.ctor === 'First') {
			return 'True';
		} else {
			return _p0._0;
		}
	}();
	return name;
};
var _elm_lang$core$Main$Second = F2(
	function (a, b) {
		return {ctor: 'Second', _0: a, _1: b};
	});
var _elm_lang$core$Main$First = {ctor: 'First'};
