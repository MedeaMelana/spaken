package spaken.ui.swing;

import java.awt.event.MouseEvent;

import spaken.model.FixedPoint;

public class PointCreateTool extends AbstractTool {

	protected PointCreateTool() {
		super("Create Point");
	}

	public void mouseClicked(MouseEvent e) {
		FixedPoint pt = new FixedPoint(e.getX(), e.getY());
		canvas.getSpace().add(pt);
		canvas.refresh();
	}

}