var _elm_lang$core$Main$apply = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Main$point = {x: 3, y: 4};
var _elm_lang$core$Main$x1 = _elm_lang$core$Main$point.x;
var _elm_lang$core$Main$x2 = function (r) {
	return r.x;
}(_elm_lang$core$Main$point);
var _elm_lang$core$Main$x3 = _elm_lang$core$Main$point.x;
var _elm_lang$core$Main$x4 = function (r) {
	return r.x;
}(_elm_lang$core$Main$point);
var _elm_lang$core$Main$x5 = A2(
	_elm_lang$core$Main$apply,
	function (r) {
		return r.x;
	},
	_elm_lang$core$Main$point);
