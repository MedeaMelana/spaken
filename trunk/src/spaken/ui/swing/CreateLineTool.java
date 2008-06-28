package spaken.ui.swing;

import java.awt.Graphics2D;
import java.awt.event.MouseEvent;

import spaken.model.*;
import spaken.model.rendered.*;

public class CreateLineTool extends AbstractTool {

	private Point p1;

	private Pos mouse;

	protected CreateLineTool() {
		super("Create Line");
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		Pos mouse = new Pos(e.getX(), e.getY());
		Point p = canvas.getSpace().getPointAt(mouse);
		if (p == null) {
			return;
		}
		if (p1 == null) {
			p1 = p;
		} else {
			canvas.getSpace().add(new Line(p1, p));
			p1 = null;
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
		highlightPoint(g, pixelSize, p1);
		if (mouse == null) {
			return;
		}
		try {
			if (p1 != null) {
				new RenderedLine(p1.getPos(), mouse, DrawingConstants.OUTLINE)
						.draw(g, pixelSize);
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
		p1 = null;
	}

}
