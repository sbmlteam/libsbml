#
# This file was converted from libsbml/src/sbml/test/TestListOf.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestListOf < Test::Unit::TestCase

  def test_ListOf_create
    lo = LibSBML::ListOf.new
    assert_equal LibSBML::SBML_LIST_OF, lo.getTypeCode
    assert_equal nil, lo.getNotes
    assert_equal nil, lo.getAnnotation
    assert_equal "", lo.getMetaId
    assert_equal 0, lo.size
  end

  def test_ListOf_free_NULL
  end

end
