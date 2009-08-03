package spaken.model.elements;

import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;
import java.util.Set;

import spaken.model.Dependency;
import spaken.model.Pos;
import spaken.ui.swing.DrawingConstants;

public class Circle extends AbstractElement<Circle> implements Dependency<Point> {

	private Point center;

	private Point distFrom;

	private Point distTo;

	/**
	 * Only used internally for reading and writing!
	 */
	public Circle() {
	}

	public Circle(Point center, Point distFrom, Point distTo) {
		this.center = center;
		this.distFrom = distFrom;
		this.distTo = distTo;
		
		center.addDependency(this);
		distFrom.addDependency(this);
		distTo.addDependency(this);
	}

	public Point getCenter() {
		return center;
	}

	public Point getDistFrom() {
		return distFrom;
	}

	public Point getDistTo() {
		return distTo;
	}

//	public Rendered render(PointBinding<Pos> binding)
//			throws ImaginaryPointException, UnboundPointException {
//		return new RenderedCircle(center.getPos(binding), distTo
//				.getPos(binding).distance(distFrom.getPos(binding)));
//	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		center.collectAssumptions(collect);
		distFrom.collectAssumptions(collect);
		distTo.collectAssumptions(collect);
	}

//	public Circle instantiate(PointBinding<Point> binding)
//			throws UnboundPointException {
//		return new Circle(center.instantiate(binding), distFrom
//				.instantiate(binding), distTo.instantiate(binding));
//	}
//
//	public void writeElement(ElementWriter out) throws IOException {
//		out.writeRef(center);
//		out.writeRef(distFrom);
//		out.writeRef(distTo);
//	}
//
//	public void readElement(ElementReader in) throws IOException {
//		center = (Point) in.readRef();
//		distFrom = (Point) in.readRef();
//		distTo = (Point) in.readRef();
//	}

	public void elementChanged(Point e) {
		notifyDependencies(this);
	}

	public void draw(Graphics2D g, double pixelSize) {
		Pos cent = center.getPos();
		Pos dfr  = distFrom.getPos();
		Pos dto  = distTo.getPos();
		
		// TODO hier moeten we iets slimmers voor bedenken
		if (cent == null || dfr == null || dto == null) return;
		
		double radius = dfr.distance(dto);
		double diam = 2 * radius;
		g.setColor(DrawingConstants.FOREGROUND);
		g.draw(new Ellipse2D.Double(cent.x - radius, cent.y - radius, diam,
				diam));
	}
	
}
