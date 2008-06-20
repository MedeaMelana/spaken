/* Created on Jun 20, 2008. */
package spaken.model;

/**
 * @author Martijn van Steenbergen
 */
public class CircleFromPoints extends Construction {

	private Point distFrom;
	private Point distTo;
	private Point center;

	public CircleFromPoints(Point distFrom, Point distTo, Point center) {
		this.distFrom = distFrom;
		this.distTo = distTo;
		this.center = center;
	}

	public Point getDistFrom() {
		return distFrom;
	}

	public Point getDistTo() {
		return distTo;
	}

	public Point getCenter() {
		return center;
	}

	@Override
	public void execute(Space space) {
		// TODO Auto-generated method stub

	}

}
