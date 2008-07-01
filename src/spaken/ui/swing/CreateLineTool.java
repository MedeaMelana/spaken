package spaken.ui.swing;

import java.awt.Graphics2D;
import java.awt.event.MouseEvent;

import spaken.model.*;
import spaken.model.rendered.*;

public class CreateLineTool extends AbstractTool {

	private Point p1;

	protected CreateLineTool() {
		super("Create Line");
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		super.mouseReleased(e);
		
		Point p = getSpace().getPointAt(getMouse());
		if (p == null) {
			return;
		}
		if (p1 == null) {
			p1 = p;
		} else {
			addElement(new Line(p1, p));
			p1 = null;
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
		super.mouseMoved(e);
		getCanvas().refresh();
	}
	
	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		highlightPoint(g, pixelSize, p1);
		
		Pos mouse = getMouse();
		
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
	}
	
	@Override
	public void resetState() {
		super.resetState();
		p1 = null;
	}

}
