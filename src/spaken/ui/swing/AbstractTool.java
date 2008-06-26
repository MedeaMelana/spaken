package spaken.ui.swing;

import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

public abstract class AbstractTool implements Tool, MouseListener,
		MouseMotionListener {

	protected SpaceCanvas canvas;
	private String name;

	protected AbstractTool(String name) {
		this.name = name;
	}

	public void install(SpaceCanvas canvas) {
		this.canvas = canvas;
		canvas.addMouseListener(this);
		canvas.addMouseMotionListener(this);
	}

	public void uninstall(SpaceCanvas canvas) {
		this.canvas = null;
		canvas.removeMouseListener(this);
		canvas.removeMouseMotionListener(this);
	}

	public String getName() {
		return name;
	}

	public void mouseDragged(MouseEvent e) {
	}

	public void mouseMoved(MouseEvent e) {
	}

	public void mouseClicked(MouseEvent e) {
	}

	public void mouseEntered(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {
	}

	public void mousePressed(MouseEvent e) {
	}

	public void mouseReleased(MouseEvent e) {
	}

	public void drawState(Graphics2D g, double pixelSize) {
	}

}
