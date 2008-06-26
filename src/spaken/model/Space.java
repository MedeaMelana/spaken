/* Created on Jun 20, 2008. */
package spaken.model;

import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;

/**
 * @author Martijn van Steenbergen
 */
public class Space {

	private List<Element> elements;

	public Space() {
		elements = new LinkedList<Element>();
		demo();
	}

	private void demo() {
		FixedPoint p1 = new FixedPoint(100, 100);
		FixedPoint p2 = new FixedPoint(200, 150);
		Line l = new Line(p1, p2);

		addElement(p1);
		addElement(p2);
		addElement(l);
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

		Collections.sort(rs, new Comparator<Rendered>() {

			public int compare(Rendered o1, Rendered o2) {
				if (o1.getClass() == o2.getClass()) {
					return 0;
				} else if (o1 instanceof RenderedPoint) {
					return 1;
				} else if (o2 instanceof RenderedPoint) {
					return -1;
				} else {
					return 0;
				}
			}
		});

		return rs;
	}

}
