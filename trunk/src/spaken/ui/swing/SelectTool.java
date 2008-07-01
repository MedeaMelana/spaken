package spaken.ui.swing;

import java.awt.Point;
import java.awt.event.MouseEvent;

public class SelectTool extends AbstractTool {

	private Point mouse;

	protected SelectTool() {
		super("Select");
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		Point newMouse = e.getPoint();
		int dx = newMouse.x - mouse.x;
		int dy = newMouse.y - mouse.y;
		double sx = canvas.getTransform().getScaleX();
		double sy = canvas.getTransform().getScaleY();
		canvas.getTransform().translate(dx / sx, dy / sy);
		mouse = newMouse;
		canvas.refresh();
	}

	@Override
	public void mousePressed(MouseEvent e) {
		mouse = e.getPoint();
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		if (e.getClickCount() > 1) {
			canvas.getTransform().scale(2, 2);
			canvas.refresh();
		}
	}

}
