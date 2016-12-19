var _elm_lang$core$Main_ops = _elm_lang$core$Main_ops || {};
_elm_lang$core$Main_ops['-'] = F2(
	function (x, y) {
		return x;
	});
var _elm_lang$core$Main$minus = A2(_elm_lang$core$Main_ops['-'], 3, 4);
var _elm_lang$core$Main$minusNegative = A2(
	_elm_lang$core$Main_ops['-'],
	3,
	A2(_elm_lang$core$Main_ops['-'], 0, 4));
var _elm_lang$core$Main$negativeMinusNegative = A2(
	_elm_lang$core$Main_ops['-'],
	A2(_elm_lang$core$Main_ops['-'], 0, 3),
	A2(_elm_lang$core$Main_ops['-'], 0, 4));
var _elm_lang$core$Main$negativeInParens = A2(_elm_lang$core$Main_ops['-'], 0, 2);
var _elm_lang$core$Main$funcMinus = A2(
	F2(
		function (x, y) {
			return A2(_elm_lang$core$Main_ops['-'], x, y);
		}),
	3,
	4);
