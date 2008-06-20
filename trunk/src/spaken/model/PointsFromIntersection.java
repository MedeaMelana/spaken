/* Created on Jun 20, 2008. */
package spaken.model;

/**
 * @author Martijn van Steenbergen
 */
public class PointsFromIntersection extends Construction {

	private Element e1;
	private Element e2;

	public PointsFromIntersection(Element e1, Element e2) {
		super();
		this.e1 = e1;
		this.e2 = e2;
	}

	public Element getE1() {
		return e1;
	}

	public Element getE2() {
		return e2;
	}

	@Override
	public void execute(Space space) {
		// TODO Auto-generated method stub

	}

}
