package xacml

import (
	"testing"
)

func TestListOfStringAttrReturnEmptyOnNilInput(t *testing.T) {
	got := listOfStringAttr(nil)
	if got == nil || len(got) != 0 {
		t.Errorf("want empty; got: %v", got)
	}
}

var emptyDoc = `<?xml version="1.0" encoding="UTF-8"?>
`

func TestEmptyDoc(t *testing.T) {
	got := emptyNode().writeDoc()
	if got != emptyDoc {
		t.Errorf("want: \n%s\ngot: \n%s\n", emptyDoc, got)
	}
}

var emptyRequest = `<?xml version="1.0" encoding="UTF-8"?>
<Request xmlns="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17" CombinedDecision="false" ReturnPolicyIdList="false"/>
`

func TestEmptyRequest(t *testing.T) {
	got := request().writeDoc()
	if got != emptyRequest {
		t.Errorf("want: \n%s\ngot: \n%s\n", emptyRequest, got)
	}
}

func TestChildrenWithEmptySlice(t *testing.T) {
	got := request().children([]xNode{}...).writeDoc()
	if got != emptyRequest {
		t.Errorf("want: \n%s\ngot: \n%s\n", emptyRequest, got)
	}
}

func TestChildrenWithOneEmptyNode(t *testing.T) {
	got := request().children(emptyNode()).writeDoc()
	if got != emptyRequest {
		t.Errorf("want: \n%s\ngot: \n%s\n", emptyRequest, got)
	}
}

func TestChildrenWithAllEmptyNodes(t *testing.T) {
	got := request().children(emptyNode(), emptyNode()).writeDoc()
	if got != emptyRequest {
		t.Errorf("want: \n%s\ngot: \n%s\n", emptyRequest, got)
	}
}

func TestChildrenWithSomeEmptyNodes(t *testing.T) {
	got := request().children(
		emptyNode(), attrCategory("c"), emptyNode()).writeDoc()
	if got != requestWithEmptyCategory {
		t.Errorf("want: \n%s\ngot: \n%s\n", requestWithEmptyCategory, got)
	}
}

var requestWithEmptyCategory = `<?xml version="1.0" encoding="UTF-8"?>
<Request xmlns="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17" CombinedDecision="false" ReturnPolicyIdList="false">
  <Attributes Category="c"/>
</Request>
`

func TestRequestWithEmptyCategory(t *testing.T) {
	got := request().children(attrCategory("c")).writeDoc()
	if got != requestWithEmptyCategory {
		t.Errorf("want: \n%s\ngot: \n%s\n", requestWithEmptyCategory, got)
	}
}

var requestWithCategoryWithEmptyAttr = `<?xml version="1.0" encoding="UTF-8"?>
<Request xmlns="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17" CombinedDecision="false" ReturnPolicyIdList="false">
  <Attributes Category="c">
    <Attribute AttributeId="a" IncludeInResult="false"/>
  </Attributes>
</Request>
`

func TestRequestWithCategoryWithEmptyAttr(t *testing.T) {
	got := request().children(
		attrCategory("c").children(
			attr("a"),
		),
	).writeDoc()
	if got != requestWithCategoryWithEmptyAttr {
		t.Errorf("want: \n%s\ngot: \n%s\n",
			requestWithCategoryWithEmptyAttr, got)
	}
}

var requestWithSomeCategories = `<?xml version="1.0" encoding="UTF-8"?>
<Request xmlns="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17" CombinedDecision="false" ReturnPolicyIdList="false">
  <Attributes Category="c">
    <Attribute AttributeId="a" IncludeInResult="false"/>
  </Attributes>
  <Attributes Category="d">
    <Attribute AttributeId="a" IncludeInResult="false"/>
    <Attribute AttributeId="b" IncludeInResult="false"/>
  </Attributes>
</Request>
`

func TestRequestWithSomeCategories(t *testing.T) {
	got := request().children(
		attrCategory("c").children(
			attr("a"),
		),
		attrCategory("d").children(
			attr("a"),
			attr("b"),
		),
	).writeDoc()
	if got != requestWithSomeCategories {
		t.Errorf("want: \n%s\ngot: \n%s\n", requestWithSomeCategories, got)
	}
}

var emptyStringAttr = `<?xml version="1.0" encoding="UTF-8"?>
<Attribute AttributeId="a" IncludeInResult="false">
  <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string"/>
</Attribute>
`

func TestEmptyStringAttr(t *testing.T) {
	got := attr("a").children(
		stringAttr(""),
	).writeDoc()
	if got != emptyStringAttr {
		t.Errorf("want: \n%s\ngot: \n%s\n", emptyStringAttr, got)
	}
}

var singletonStringAttr = `<?xml version="1.0" encoding="UTF-8"?>
<Attribute AttributeId="a" IncludeInResult="false">
  <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">1</AttributeValue>
</Attribute>
`

func TestSingletonStringAttr(t *testing.T) {
	got := attr("a").children(
		stringAttr("1"),
	).writeDoc()
	if got != singletonStringAttr {
		t.Errorf("want: \n%s\ngot: \n%s\n", singletonStringAttr, got)
	}
}

var listOfStrAttr = `<?xml version="1.0" encoding="UTF-8"?>
<Attribute AttributeId="a" IncludeInResult="false">
  <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">1</AttributeValue>
  <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">2</AttributeValue>
</Attribute>
`

func TestListOfStringAttr(t *testing.T) {
	got := attr("a").children(
		listOfStringAttr([]string{"1", "2"})...,
	).writeDoc()
	if got != listOfStrAttr {
		t.Errorf("want: \n%s\ngot: \n%s\n", listOfStrAttr, got)
	}
}

var boolAttrs = `<?xml version="1.0" encoding="UTF-8"?>
<Attribute AttributeId="a" IncludeInResult="false">
  <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#boolean">false</AttributeValue>
  <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#boolean">true</AttributeValue>
</Attribute>
`

func TestBoolAttrs(t *testing.T) {
	got := attr("a").children(
		boolAttr(false),
		boolAttr(true),
	).writeDoc()
	if got != boolAttrs {
		t.Errorf("want: \n%s\ngot: \n%s\n", boolAttrs, got)
	}
}

var uriAttrs = `<?xml version="1.0" encoding="UTF-8"?>
<Attribute AttributeId="a" IncludeInResult="false">
  <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#anyURI"/>
  <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#anyURI">some.host</AttributeValue>
</Attribute>
`

func TestUriAttrs(t *testing.T) {
	got := attr("a").children(
		uriAttr(""),
		uriAttr("some.host"),
	).writeDoc()
	if got != uriAttrs {
		t.Errorf("want: \n%s\ngot: \n%s\n", uriAttrs, got)
	}
}
