#define BOOST_TEST_MODULE pcraster dal memory_table_driver
#include <boost/test/unit_test.hpp>
#include "dal_MemoryDataPool.h"
#include "dal_MemoryTableDriver.h"
#include "dal_Table.h"


struct Fixture
{

    Fixture()
    {
        using namespace dal;

        std::string title;
        std::vector<std::string> titles;
        std::vector<TypeId> typeIds;

        title = "Table1";
        titles.push_back("UINT1");
        titles.push_back("INT4");
        titles.push_back("NR_TYPE_IDS");
        titles.push_back("REAL4");
        typeIds.push_back(TI_UINT1);
        typeIds.push_back(TI_INT4);
        typeIds.push_back(TI_NR_TYPES);
        typeIds.push_back(TI_REAL4);
        d_table1 = new Table(title, titles, typeIds);

        d_table1->createCols();
        Array<UINT1> col1(d_table1->col<UINT1>(0));
        Array<INT4> col2(d_table1->col<INT4>(1));
        Array<REAL4> col4(d_table1->col<REAL4>(3));
        col1.push_back(1);
        col1.push_back(0);
        col1.push_back(1);
        col2.push_back(1);
        col2.push_back(2);
        col2.push_back(3);
        col4.push_back(REAL4(1.1));
        col4.push_back(REAL4(2.2));
        col4.push_back(REAL4(3.3));

        title = "Table2";
        titles.clear();
        typeIds.clear();
        d_table2 = new Table(title, titles, typeIds);

        title = "Table3";
        titles.clear();
        typeIds.clear();
        d_table3 = new Table(title, titles, typeIds);
    }

    ~Fixture()
    {
        delete d_table3;
        delete d_table2;
        delete d_table1;
    }


    dal::Table*    d_table1;
    dal::Table*    d_table2;
    dal::Table*    d_table3;

};


BOOST_FIXTURE_TEST_SUITE(memory_table_driver, Fixture)

BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  MemoryDataPool pool;
  MemoryTableDriver driver(&pool);

  BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists("table1"));

  dynamic_cast<TableDriver&>(driver).write(*d_table1, "table1");
  BOOST_CHECK(dynamic_cast<Driver&>(driver).exists("table1"));

  Table* table = dynamic_cast<TableDriver&>(driver).read("table1");
  BOOST_CHECK(*d_table1 == *table);
  BOOST_CHECK(d_table1 != table);
  delete table;
}

BOOST_AUTO_TEST_SUITE_END()
