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

	protected CreateCircleTool() {
		super("Create Circle");
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		super.mouseReleased(e);
		
		Point p = getSpace().getPointAt(getMouse(),
				getCanvas().getPointSelectSize());
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
		getCanvas().refresh();
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
	public void uninstall(SpaceCanvas canvas) {
		super.uninstall(canvas);
	}

	@Override
	public void resetState() {
		super.resetState();
		distFrom = null;
		distTo = null;
	}

}
