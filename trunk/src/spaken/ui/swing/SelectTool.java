package spaken.ui.swing;

import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;

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
	public void mouseWheelMoved(MouseWheelEvent e) {
		int rot = e.getWheelRotation();
		scale(-0.5 * rot);
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		if (e.getClickCount() > 1) {
			if (e.getButton() == MouseEvent.BUTTON3) {
				scale(-1);
			} else {
				scale(1);
			}
		}
	}

	private void scale(double zoom) {
		double scale = Math.pow(2, zoom);
		canvas.getTransform().scale(scale, scale);
		canvas.refresh();
	}
}
