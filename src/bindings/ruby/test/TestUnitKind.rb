#
# This file was converted from libsbml/src/sbml/test/TestUnitKind.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestUnitKind < Test::Unit::TestCase

  def test_UnitKind_isValidUnitKindString
    assert_equal 0, LibSBML::UnitKind_isValidUnitKindString("fun-foam-unit for kids!",1,1)
    assert_equal 1, LibSBML::UnitKind_isValidUnitKindString("litre",2,2)
    assert_equal 0, LibSBML::UnitKind_isValidUnitKindString("liter",2,2)
    assert_equal 1, LibSBML::UnitKind_isValidUnitKindString("liter",1,2)
    assert_equal 0, LibSBML::UnitKind_isValidUnitKindString("meter",2,3)
    assert_equal 1, LibSBML::UnitKind_isValidUnitKindString("metre",2,1)
    assert_equal 1, LibSBML::UnitKind_isValidUnitKindString("meter",1,2)
    assert_equal 1, LibSBML::UnitKind_isValidUnitKindString("Celsius",2,1)
    assert_equal 0, LibSBML::UnitKind_isValidUnitKindString("Celsius",2,2)
  end

  def test_UnitKind_toString
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_AMPERE)
    assert_equal  "ampere",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_BECQUEREL)
    assert_equal  "becquerel",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_CANDELA)
    assert_equal  "candela",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_CELSIUS)
    assert_equal  "Celsius",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_COULOMB)
    assert_equal  "coulomb",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_DIMENSIONLESS)
    assert_equal  "dimensionless",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_FARAD)
    assert_equal  "farad",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_GRAM)
    assert_equal  "gram",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_GRAY)
    assert_equal  "gray",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_HENRY)
    assert_equal  "henry",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_HERTZ)
    assert_equal  "hertz",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_ITEM)
    assert_equal  "item",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_JOULE)
    assert_equal  "joule",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_KATAL)
    assert_equal  "katal",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_KELVIN)
    assert_equal  "kelvin",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_KILOGRAM)
    assert_equal  "kilogram",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_LITER)
    assert_equal  "liter",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_LITRE)
    assert_equal  "litre",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_LUMEN)
    assert_equal  "lumen",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_LUX)
    assert_equal  "lux",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_METER)
    assert_equal  "meter",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_METRE)
    assert_equal  "metre",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_MOLE)
    assert_equal  "mole",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_NEWTON)
    assert_equal  "newton",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_OHM)
    assert_equal  "ohm",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_PASCAL)
    assert_equal  "pascal",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_RADIAN)
    assert_equal  "radian",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_SECOND)
    assert_equal  "second",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_SIEMENS)
    assert_equal  "siemens",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_SIEVERT)
    assert_equal  "sievert",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_STERADIAN)
    assert_equal  "steradian",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_TESLA)
    assert_equal  "tesla",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_VOLT)
    assert_equal  "volt",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_WATT)
    assert_equal  "watt",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_WEBER)
    assert_equal  "weber",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_INVALID)
    assert_equal  "(Invalid UnitKind)",s
    s = LibSBML::UnitKind_toString(-1)
    assert_equal  "(Invalid UnitKind)",s
    s = LibSBML::UnitKind_toString(LibSBML::UNIT_KIND_INVALID + 1)
    assert_equal  "(Invalid UnitKind)",s
  end

  def test_UnitKind_forName
    assert_equal LibSBML::UNIT_KIND_AMPERE, LibSBML::UnitKind_forName("ampere")
    assert_equal LibSBML::UNIT_KIND_BECQUEREL, LibSBML::UnitKind_forName("becquerel")
    assert_equal LibSBML::UNIT_KIND_CANDELA, LibSBML::UnitKind_forName("candela")
    assert_equal LibSBML::UNIT_KIND_CELSIUS, LibSBML::UnitKind_forName("Celsius")
    assert_equal LibSBML::UNIT_KIND_COULOMB, LibSBML::UnitKind_forName("coulomb")
    assert_equal LibSBML::UNIT_KIND_DIMENSIONLESS, LibSBML::UnitKind_forName("dimensionless")
    assert_equal LibSBML::UNIT_KIND_FARAD, LibSBML::UnitKind_forName("farad")
    assert_equal LibSBML::UNIT_KIND_GRAM, LibSBML::UnitKind_forName("gram")
    assert_equal LibSBML::UNIT_KIND_GRAY, LibSBML::UnitKind_forName("gray")
    assert_equal LibSBML::UNIT_KIND_HENRY, LibSBML::UnitKind_forName("henry")
    assert_equal LibSBML::UNIT_KIND_HERTZ, LibSBML::UnitKind_forName("hertz")
    assert_equal LibSBML::UNIT_KIND_ITEM, LibSBML::UnitKind_forName("item")
    assert_equal LibSBML::UNIT_KIND_JOULE, LibSBML::UnitKind_forName("joule")
    assert_equal LibSBML::UNIT_KIND_KATAL, LibSBML::UnitKind_forName("katal")
    assert_equal LibSBML::UNIT_KIND_KELVIN, LibSBML::UnitKind_forName("kelvin")
    assert_equal LibSBML::UNIT_KIND_KILOGRAM, LibSBML::UnitKind_forName("kilogram")
    assert_equal LibSBML::UNIT_KIND_LITER, LibSBML::UnitKind_forName("liter")
    assert_equal LibSBML::UNIT_KIND_LITRE, LibSBML::UnitKind_forName("litre")
    assert_equal LibSBML::UNIT_KIND_LUMEN, LibSBML::UnitKind_forName("lumen")
    assert_equal LibSBML::UNIT_KIND_LUX, LibSBML::UnitKind_forName("lux")
    assert_equal LibSBML::UNIT_KIND_METER, LibSBML::UnitKind_forName("meter")
    assert_equal LibSBML::UNIT_KIND_METRE, LibSBML::UnitKind_forName("metre")
    assert_equal LibSBML::UNIT_KIND_MOLE, LibSBML::UnitKind_forName("mole")
    assert_equal LibSBML::UNIT_KIND_NEWTON, LibSBML::UnitKind_forName("newton")
    assert_equal LibSBML::UNIT_KIND_OHM, LibSBML::UnitKind_forName("ohm")
    assert_equal LibSBML::UNIT_KIND_PASCAL, LibSBML::UnitKind_forName("pascal")
    assert_equal LibSBML::UNIT_KIND_RADIAN, LibSBML::UnitKind_forName("radian")
    assert_equal LibSBML::UNIT_KIND_SECOND, LibSBML::UnitKind_forName("second")
    assert_equal LibSBML::UNIT_KIND_SIEMENS, LibSBML::UnitKind_forName("siemens")
    assert_equal LibSBML::UNIT_KIND_SIEVERT, LibSBML::UnitKind_forName("sievert")
    assert_equal LibSBML::UNIT_KIND_STERADIAN, LibSBML::UnitKind_forName("steradian")
    assert_equal LibSBML::UNIT_KIND_TESLA, LibSBML::UnitKind_forName("tesla")
    assert_equal LibSBML::UNIT_KIND_VOLT, LibSBML::UnitKind_forName("volt")
    assert_equal LibSBML::UNIT_KIND_WATT, LibSBML::UnitKind_forName("watt")
    assert_equal LibSBML::UNIT_KIND_WEBER, LibSBML::UnitKind_forName("weber")
    assert_equal LibSBML::UNIT_KIND_INVALID, LibSBML::UnitKind_forName(nil)
    assert_equal LibSBML::UNIT_KIND_INVALID, LibSBML::UnitKind_forName("")
    assert_equal LibSBML::UNIT_KIND_INVALID, LibSBML::UnitKind_forName("foobar")
  end

  def test_UnitKind_equals
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_AMPERE,LibSBML::UNIT_KIND_AMPERE)
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_INVALID,LibSBML::UNIT_KIND_INVALID)
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_LITER,LibSBML::UNIT_KIND_LITER)
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_LITRE,LibSBML::UNIT_KIND_LITRE)
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_METER,LibSBML::UNIT_KIND_METER)
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_METRE,LibSBML::UNIT_KIND_METRE)
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_LITER,LibSBML::UNIT_KIND_LITRE)
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_LITRE,LibSBML::UNIT_KIND_LITER)
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_METER,LibSBML::UNIT_KIND_METRE)
    assert_equal 1, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_METRE,LibSBML::UNIT_KIND_METER)
    assert_equal 0, LibSBML::UnitKind_equals(LibSBML::UNIT_KIND_AMPERE,LibSBML::UNIT_KIND_WEBER)
  end

end
