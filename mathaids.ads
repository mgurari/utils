package mathAids is

	type dataArr is array(integer range<>) of float;
	type resultArr is array(positive range<>,integer range<>) of float;

	function diff(X1,Y1:dataArr) return resultArr;
	function integ(X2,Y2:dataArr;takeAbs:boolean) return float;

end mathAids;
