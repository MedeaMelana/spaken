package spaken.ui.swing;

import java.awt.Graphics2D;

import spaken.model.Circle;
import spaken.model.ImaginaryPointException;
import spaken.model.Point;
import spaken.model.Pos;
import spaken.model.rendered.RenderedCircle;

// Idea:
// One stroke creates a circle from two points.
// Three stokes create a circle from three points.
public class CreateCircleTool extends AbstractTool {

	private Point distFrom, distTo;

	protected CreateCircleTool() {
		super("Create Circle");
	}

	@Override
	protected void mouseMoved(Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeStarted(Pos origin) {
		if (distFrom == null) {
			distFrom = getCanvas().getPointAt(origin);
		}
	}

	@Override
	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		Point p = getCanvas().getPointAt(end);

		if (p == null) {
			return;
		} else if (distTo != null) {
			addElement(new Circle(p, distFrom, distTo));
			resetState();
		} else if (distFrom != null && distFrom != p) {
			distTo = p;
		} else if (distFrom == null) {
			distFrom = p;
		}
		getCanvas().refresh();
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		highlightPoint(g, pixelSize, distFrom);
		highlightPoint(g, pixelSize, distTo);

		Pos mouse = getMouse();

		if (mouse == null) {
			return;
		}

		try {
			if (distTo != null) {
				new RenderedCircle(mouse, distTo.getPos().distance(
						distFrom.getPos()), DrawingConstants.OUTLINE).draw(g,
						pixelSize);
			} else if (distFrom != null) {
				new RenderedCircle(mouse, distFrom.getPos().distance(mouse),
						DrawingConstants.OUTLINE).draw(g, pixelSize);
			}
		} catch (ImaginaryPointException e) {
		}
	}

	@Override
	public void resetState() {
		super.resetState();
		distFrom = null;
		distTo = null;
	}

}
