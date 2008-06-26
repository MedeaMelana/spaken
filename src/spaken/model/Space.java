/* Created on Jun 20, 2008. */
package spaken.model;

import java.util.LinkedList;
import java.util.List;

import spaken.model.rendered.Rendered;

/**
 * @author Martijn van Steenbergen
 */
public class Space {

	private List<Element> elements;

	public Space() {
		elements = new LinkedList<Element>();
	}

	public void addElement(Element e) {
		elements.add(e);
	}

	public Iterable<Element> getElements() {
		return elements;
	}

	public Iterable<Rendered> render() {
		List<Rendered> rs = new LinkedList<Rendered>();
		for (Element e : elements) {
			try {
				rs.add(e.render());
			} catch (ImaginaryPointException e1) {
				System.out.println(e1);
			}
		}
		return rs;
	}

}
