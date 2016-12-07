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
	function (v1, v2) {
		return {ctor: 'Second', _0: v1, _1: v2};
	});
var _elm_lang$core$Main$First = {ctor: 'First'};
