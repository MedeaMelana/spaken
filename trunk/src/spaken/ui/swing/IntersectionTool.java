package spaken.ui.swing;

import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.util.*;

import spaken.model.*;
import spaken.model.intersections.Intersections;
import spaken.model.rendered.RenderedPoint;
import spaken.util.Iterables;

public class IntersectionTool extends AbstractTool {

	private List<CachedPoint> intersections;

	protected IntersectionTool() {
		super("Mark Intersection");
		intersections = new LinkedList<CachedPoint>();
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		super.mouseReleased(e);

		CachedPoint pt = closestPoint();
		if (pt == null)
			return;

		addElement(pt.getDynamicPoint());
	}

	private CachedPoint closestPoint() {
		return getSpace().getPointAt(getMouse(), intersections,
				getCanvas().getPointSelectSize());
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		super.mouseMoved(e);

		getCanvas().refresh();
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		super.mouseDragged(e);

		getCanvas().refresh();
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		Pos mouse = getMouse();

		if (mouse != null) {
			Point pt = closestPoint();
			if (pt == null)
				return;

			try {
				new RenderedPoint(pt.getPos(), true, DrawingConstants.OUTLINE)
						.draw(g, pixelSize);
			} catch (ImaginaryPointException e) {
			}
		}
	}

	@Override
	public void install(SpaceCanvas canvas) {
		super.install(canvas);

		intersections.clear();

		Iterable<Element> elements = canvas.getSpace().getElements();
		List<Element> others = new LinkedList<Element>();
		Iterables.addAll(others, elements);

		// Calculate and store all possible intersections.
		for (Element e : elements) {
			others.remove(e);
			for (Element o : others) {
				for (Point p : Intersections.intersections(e, o)) {
					intersections.add(new CachedPoint(p));
				}
			}
		}

	}

	@Override
	public void uninstall(SpaceCanvas canvas) {
		super.uninstall(canvas);
		intersections.clear();
	}

	private static class CachedPoint extends DerivedPoint {

		private Point point;

		private Pos pos;

		private CachedPoint(Point point) {
			this.point = point;
		}

		public Pos getPos() throws ImaginaryPointException {
			if (pos == null) {
				pos = point.getPos();
			}
			return pos;
		}

		public Point getDynamicPoint() {
			return point;
		}

	}

}
