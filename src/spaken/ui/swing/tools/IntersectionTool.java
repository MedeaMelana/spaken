package spaken.ui.swing.tools;

import java.awt.Graphics2D;
import java.io.IOException;
import java.util.*;

import spaken.model.*;
import spaken.model.elements.*;
import spaken.model.elements.intersections.Intersections;
import spaken.model.rendered.RenderedPoint;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;
import spaken.ui.swing.DrawingConstants;
import spaken.ui.swing.SpaceCanvas;
import spaken.util.Collector;
import spaken.util.Iterables;

public class IntersectionTool extends AbstractTool {

	private List<CachedPoint> intersections;

	public IntersectionTool() {
		super("Mark Intersection");
		intersections = new LinkedList<CachedPoint>();
	}

	private CachedPoint closestPoint() {
		return getSpace().getPointAt(getMouse(), intersections,
				getCanvas().getPointSelectSize());
	}

	@Override
	protected void mouseMoved(Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		CachedPoint pt = closestPoint();
		if (pt == null) {
			return;
		}

		addElement(pt.getDynamicPoint());
	}

	@Override
	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
		getCanvas().repaint();
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		Pos mouse = getMouse();

		if (mouse != null) {
			Point pt = closestPoint();
			if (pt == null)
				return;

			try {
				new RenderedPoint(getPos(pt), RenderedPoint.Type.DERIVED,
						DrawingConstants.OUTLINE).draw(g, pixelSize);
			} catch (ImaginaryPointException e) {
			} catch (UnboundPointException e) {
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

	private static class CachedPoint extends AbstractPoint {

		private Point point;

		private Pos pos;

		private CachedPoint(Point point) {
			this.point = point;
		}

		public Pos getPos(PointBinding<Pos> binding) throws ImaginaryPointException, UnboundPointException {
			if (pos == null) {
				pos = point.getPos(binding);
			}
			return pos;
		}

		public Point getDynamicPoint() {
			return point;
		}

		private <A> A impossible(String verb) {
			throw new UnsupportedOperationException(
					"This internal CachedPoint has escaped and is now being "
							+ verb + "!");
		}

		public void readElement(ElementReader in) throws IOException {
			impossible("read");
		}

		public void writeElement(ElementWriter out) throws IOException {
			impossible("written");
		}

		public void collectAssumptions(Collector<AssumedPoint> collect) {
			impossible("collected");
		}

		public Point instantiate(PointBinding<Point> binding) throws UnboundPointException {
			return impossible("instantiated");
		}

	}

}
