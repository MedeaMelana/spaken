package spaken.ui.swing;

import java.awt.event.MouseEvent;

import spaken.model.FixedPoint;
import spaken.model.Pos;

public class PointMoveTool extends AbstractTool {

	protected PointMoveTool() {
		super("Move Point");
	}

	private Pos mouseOld;
	private FixedPoint dragging;

	public void mousePressed(MouseEvent e) {
		super.mousePressed(e);
		
		dragging = getSpace().getFixedPointAt(getMouse());
		mouseOld = getMouse();
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		super.mouseDragged(e);
		
		Pos mouse = getMouse();
		
		if (dragging != null) {
			Pos diff = mouse.subtract(mouseOld);
			dragging.setPos(dragging.getPos().add(diff));
			getCanvas().refresh();
			mouseOld = mouse;
		}
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		super.mouseReleased(e);
		
		mouseOld = null;
		dragging = null;
	}

}
