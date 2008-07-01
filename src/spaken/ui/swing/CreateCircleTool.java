package spaken.ui.swing;

import java.awt.Graphics2D;
import java.awt.event.MouseEvent;

import spaken.model.Circle;
import spaken.model.ImaginaryPointException;
import spaken.model.Point;
import spaken.model.Pos;
import spaken.model.rendered.RenderedCircle;

public class CreateCircleTool extends AbstractTool {

	private Point distFrom, distTo;

	private Pos mouse;

	protected CreateCircleTool() {
		super("Create Circle");
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		Pos mouse = new Pos(e.getX(), e.getY());
		Point p = canvas.getSpace().getPointAt(mouse);
		if (p == null) {
			return;
		}
		if (distFrom == null) {
			distFrom = p;
		} else if (distTo == null) {
			distTo = p;
		} else {
			addElement(new Circle(p, distFrom, distTo));
			distFrom = null;
			distTo = null;
		}
		canvas.refresh();
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		canvas.refresh();
		mouse = new Pos(e.getX(), e.getY());
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		mouseMoved(e);
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		highlightPoint(g, pixelSize, distFrom);
		highlightPoint(g, pixelSize, distTo);
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
	public void uninstall(SpaceCanvas canvas) {
		super.uninstall(canvas);
		mouse = null;
	}

	@Override
	public void resetState() {
		super.resetState();
		mouse = null;
		distFrom = null;
		distTo = null;
	}

}
