var _elm_lang$core$Main$any = F2(
	function (isOk, list) {
		any:
		while (true) {
			var _p0 = list;
			if (_p0.ctor === '::') {
				if (isOk(_p0._0)) {
					return true;
				} else {
					var _v1 = isOk,
						_v2 = _p0._1;
					isOk = _v1;
					list = _v2;
					continue any;
				}
			} else {
				return false;
			}
		}
	});
