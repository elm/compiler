var _elm_lang$core$Main$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Main$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Main$withDefault0 = function (maybe) {
	var _p0 = maybe;
	if (_p0.ctor === 'Just') {
		var maybe = _elm_lang$core$Main$Just('shadow argument, but do not overwrite it in JS!');
		return _p0._0;
	} else {
		return 0;
	}
};
