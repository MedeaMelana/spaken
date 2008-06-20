/* Created on Jun 20, 2008. */
package spaken.model;

/**
 * @author Martijn van Steenbergen
 */
public class LineFromPoints extends Construction {

	private Point p1;
	private Point p2;

	public LineFromPoints(Point p1, Point p2) {
		this.p1 = p1;
		this.p2 = p2;
	}

	public Point getP1() {
		return p1;
	}

	public Point getP2() {
		return p2;
	}

	@Override
	public void execute(Space space) {
		// TODO Auto-generated method stub

	}

}
