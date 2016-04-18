var _elm_lang$core$Main$test = function (x) {
	var _p0 = x;
	if (_p0 === 1) {
		return 2;
	} else {
		return _elm_lang$core$Native_Utils.crashCase(
			'Main',
			{
				start: {line: 5, column: 5},
				end: {line: 9, column: 43}
			},
			_p0)('unexpected value');
	}
};
