package xacml

import (
	"fmt"

	xml "github.com/beevik/etree"
)

// convenience wrapper to be able to add multiple child nodes at once,
// see children function.
type xNode struct {
	label *xml.Element
}

func node(label *xml.Element) xNode {
	return xNode{label: label}
}

func emptyNode() xNode {
	return xNode{}
}

func (n xNode) isEmpty() bool {
	return n.label == nil
}

// serialize current node to XML doc.
func (n xNode) writeDoc() string {
	doc := xml.NewDocument()
	doc.CreateProcInst("xml", `version="1.0" encoding="UTF-8"`)
	if !n.isEmpty() {
		doc.SetRoot(n.label)
	}

	doc.Indent(2)
	serialized, _ := doc.WriteToString()
	return serialized
}

// NOTE. Performance. For now XACML requests are pretty small, so we can
// afford to keep the whole thing in memory. In general tough a better
// option would be to stream the doc into the HTTP request as the XML
// gets written. I couldn't find any decent streaming solution in Go,
// e.g. something like Haskell pipes, conduit, etc. If workarounds are
// your thing, here's one
// - https://medium.com/stupid-gopher-tricks/streaming-data-in-go-without-buffering-3285ddd2a1e5
// but watch out for concurrency!!

// convenience function to be able to add multiple child nodes at once.
func (n xNode) children(ts ...xNode) xNode {
	for _, t := range ts {
		if !t.isEmpty() {
			n.label.AddChild(t.label)
		}
	}
	return n
}

// generic constructors for XACML request elements.
// see builderfw for specialized constructors.

func attrValue(dataType string, value interface{}) xNode {
	e := xml.NewElement("AttributeValue")
	e.CreateAttr("DataType", dataType)

	v := fmt.Sprintf("%v", value)
	e.SetText(v)

	return node(e)
}

func stringAttr(v string) xNode {
	typ := "http://www.w3.org/2001/XMLSchema#string"
	return attrValue(typ, v)
}

func listOfStringAttr(vs []string) []xNode {
	xs := make([]xNode, len(vs))
	for k, v := range vs {
		xs[k] = stringAttr(v)
	}
	return xs
}

func boolAttr(v bool) xNode {
	typ := "http://www.w3.org/2001/XMLSchema#boolean"
	return attrValue(typ, v)
}

func uriAttr(v string) xNode {
	typ := "http://www.w3.org/2001/XMLSchema#anyURI"
	return attrValue(typ, v)
}

func attr(id string) xNode {
	e := xml.NewElement("Attribute")
	e.CreateAttr("AttributeId", id)
	e.CreateAttr("IncludeInResult", "false")

	return node(e)
}

func attrCategory(id string) xNode {
	e := xml.NewElement("Attributes")
	e.CreateAttr("Category", id)

	return node(e)
}

func request() xNode {
	e := xml.NewElement("Request")
	e.CreateAttr("xmlns", "urn:oasis:names:tc:xacml:3.0:core:schema:wd-17")
	e.CreateAttr("CombinedDecision", "false")
	e.CreateAttr("ReturnPolicyIdList", "false")

	return node(e)
}
