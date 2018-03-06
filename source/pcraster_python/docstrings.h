#pragma once


const std::string cellvalue_idx_doc = R"(
   Return a cell value from a map.

   map -- Map you want to query.

   index -- Linear index of a cell in the map, ranging from
            [1, number-of-cells].

   Returns a tuple with two elements: the first is the cell value, the second
   is a boolean value which shows whether the first element, is valid or not.
   If the second element is False, the cell contains a missing value.

   See also: cellvalue(map, row, col)
)";

const std::string cellvalue_rc_doc = R"(

   Return a cell value from a map.

   map -- Map you want to query.

   row -- Row index of a cell in the map, ranging from [1, number-of-rows].

   col -- Col index of a cell in the map, ranging from [1, number-of-cols].

   Returns a tuple with two elements: the first is the cell value,
   the second is a boolean value which shows whether the first element,
   is valid or not.
   If the second element is False, the cell contains a missing value.

   See also: cellvalue(map, index)
)";

/*
const std::string _doc = R"(
)";
*/

















