var _elm_lang$core$Main$Cons = F2(
	function (a, b) {
		return {ctor: 'Cons', _0: a, _1: b};
	});
var _elm_lang$core$Main$Nil = {ctor: 'Nil'};
var _elm_lang$core$Main$zip = F2(
	function (list1, list2) {
		var _p0 = {ctor: '_Tuple2', _0: list1, _1: list2};
		if (_p0._0.ctor === 'Nil') {
			return _elm_lang$core$Main$Nil;
		} else {
			if (_p0._1.ctor === 'Nil') {
				return _elm_lang$core$Main$Nil;
			} else {
				return A2(
					_elm_lang$core$Main$Cons,
					{ctor: '_Tuple2', _0: _p0._0._0, _1: _p0._1._0},
					A2(_elm_lang$core$Main$zip, _p0._0._1, _p0._1._1));
			}
		}
	});
