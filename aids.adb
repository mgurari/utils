package body aids is

	function isInInt(char1:integer;arr1:intArr) return boolean is
	begin
		for i of arr1 loop
			if i=char1 then return true; end if;
		end loop;
		return false;
	end isInInt;


	function isInFlt(char2:float;arr2:floatArr) return boolean is
	begin
		for i of arr2 loop
			if i=char2 then return true; end if;
		end loop;
		return false;
	end isInFlt;

	
	function isInStr(char3:character;arr3:string) return boolean is
	begin
		for i of arr3 loop
			if i=char3 then return true; end if;
		end loop;
		return false;
	end isInStr;
	
	function linspace(start:float;fin:float;elems:integer) return floatArr is
		result:floatArr(1..elems);
		increment:float:=(fin-start)/float(elems);
	begin
		result(1):=start;
		result(elems):=fin;
		for i in 2..(elems-1) loop
			result(i):=result(i-1)+increment;
		end loop;
		return result;

	end linspace;

end aids;
