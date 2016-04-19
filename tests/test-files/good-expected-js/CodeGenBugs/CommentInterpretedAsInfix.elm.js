var _elm_lang$core$Main$always = F2(
	function (x, s) {
		return x;
	});
var _elm_lang$core$Main_ops = _elm_lang$core$Main_ops || {};
_elm_lang$core$Main_ops['|>'] = F2(
	function (a, f) {
		return f(a);
	});
var _elm_lang$core$Main$value = A2(
	_elm_lang$core$Main_ops['|>'],
	'Hi',
	_elm_lang$core$Main$always('Hello'));
