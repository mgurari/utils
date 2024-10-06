package body aids is

	function isInInt(char1:integer;arr1:intArr) return boolean is
	begin
		for i in 1..arr1'length loop
			if arr1(i)=char1 then return true; end if;
		end loop;
		return false;
	end isInInt;


	function isInFlt(char2:float;arr2:floatArr) return boolean is
	begin
		for i in 1..arr2'length loop
			if arr2(i)=char2 then return true; end if;
		end loop;
		return false;
	end isInFlt;

	
	function isInStr(char3:character;arr3:string) return boolean is
	begin
		for i in 1..arr3'length loop
			if arr3(i)=char3 then return true; end if;
		end loop;
		return false;
	end isInStr;

end aids;
