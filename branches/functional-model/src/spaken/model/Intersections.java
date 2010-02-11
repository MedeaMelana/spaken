package spaken.model;

import spaken.model.elements.*;

public class Intersections {

	/**
	 * Determine all intersection points between elements <t>e1</tt> and <tt>e2</tt>.
	 * 
	 * @param e1
	 * @param e2
	 * @return
	 * @throws Err 
	 */
	public static Element intersections(Construct sp, Element e1, Element e2) {
		if (e1 instanceof Line) {
			if (e2 instanceof Line) {
				return sp.intersectLL(e1, e2);
			} else if (e2 instanceof Circle) {
				return sp.intersectLC(e1, e2);
			}
		} else if (e1 instanceof Circle) {
			if (e2 instanceof Circle) {
				return sp.intersectCC(e1, e2);
			} else if (e2 instanceof Line) {
				return sp.intersectLC(e2, e1);
			}
		}
		throw new RuntimeException("Non-intersectable elements"); // TODO betere exception
	}

}
