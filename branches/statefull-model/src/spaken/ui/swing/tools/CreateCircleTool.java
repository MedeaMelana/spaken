package spaken.ui.swing.tools;

import java.awt.Graphics2D;

import spaken.model.Pos;
import spaken.model.elements.Circle;
import spaken.model.elements.Point;

// Idea:
// One stroke creates a circle from two points.
// Three stokes create a circle from three points.
public class CreateCircleTool extends AbstractTool {

	private Point distFrom, distTo;

	public CreateCircleTool() {
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

		if (isMouseInside()) {
			if (distTo != null) {
				// TODO outlinekleur
				new Circle(getMousePoint(), distFrom, distTo).draw(g, pixelSize);
			} else if (distFrom != null) {
				// TODO outlinekleur
				new Circle(getMousePoint(), distFrom, getMousePoint()).draw(g,pixelSize);
			}
		}
	}

	@Override
	public void resetState() {
		super.resetState();
		distFrom = null;
		distTo = null;
	}

}
