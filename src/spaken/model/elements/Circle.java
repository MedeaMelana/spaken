package spaken.model.elements;

import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;
import java.util.Collection;

import spaken.model.*;
import spaken.ui.swing.DrawingConstants;
import spaken.util.Collector;

public class Circle extends AbstractElement<Circle> implements
		ElementListener<Point> {

	private Point<?> center;

	private Point<?> distFrom;

	private Point<?> distTo;

	/**
	 * Only used internally for reading and writing!
	 */
	public Circle() {
	}

	public Circle(Point<?> center, Point<?> distFrom, Point<?> distTo) {
		this.center = center;
		this.distFrom = distFrom;
		this.distTo = distTo;

		center.addElementListener(this);
		distFrom.addElementListener(this);
		distTo.addElementListener(this);
	}

	protected Circle duplicateSub() {
		return new Circle(center.duplicate(), distFrom.duplicate(), distTo
				.duplicate());
	}

	public void collectDependencies(Collection<Element<?>> collect) {
		collect.add(center);
		collect.add(distFrom);
		collect.add(distTo);
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

	// public Rendered render(PointBinding<Pos> binding)
	// throws ImaginaryPointException, UnboundPointException {
	// return new RenderedCircle(center.getPos(binding), distTo
	// .getPos(binding).distance(distFrom.getPos(binding)));
	// }

	public void collectAssumptions(Collector<AssumedPoint> collect) {
		center.collectAssumptions(collect);
		distFrom.collectAssumptions(collect);
		distTo.collectAssumptions(collect);
	}

	// public Circle instantiate(PointBinding<Point> binding)
	// throws UnboundPointException {
	// return new Circle(center.instantiate(binding), distFrom
	// .instantiate(binding), distTo.instantiate(binding));
	// }
	//
	// public void writeElement(ElementWriter out) throws IOException {
	// out.writeRef(center);
	// out.writeRef(distFrom);
	// out.writeRef(distTo);
	// }
	//
	// public void readElement(ElementReader in) throws IOException {
	// center = (Point) in.readRef();
	// distFrom = (Point) in.readRef();
	// distTo = (Point) in.readRef();
	// }

	public void elementChanged(Point e) {
		notifyElementListeners(this);
	}

	public void draw(Graphics2D g, double pixelSize) {
		try {
			Pos cent = center.getPos();

			double radius = distFrom.getPos().distance(distTo.getPos());
			double diam = 2 * radius;
			g.setColor(DrawingConstants.FOREGROUND);
			g.draw(new Ellipse2D.Double(cent.x - radius, cent.y - radius, diam,
					diam));
		} catch (ImaginaryPointException e) {
			return;
		}
	}

}
