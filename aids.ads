package aids is

	type intArr is array(integer range<>) of integer;
	type floatArr is array(integer range<>) of float;

	function isInInt(char1:integer; arr1:intArr) return boolean;
	function isInFlt(char2:float; arr2:floatArr) return boolean;
	function isInStr(char3:character; arr3:string) return boolean;
	function linspace(start:float;fin:float;elems:integer) return floatArr;

end aids;
