/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import javax.swing.JPanel;

import spaken.model.Space;
import spaken.model.rendered.Rendered;

/**
 * @author Martijn van Steenbergen
 */
public class SpaceCanvas extends JPanel {

	private Space space;

	private Tool currentTool;

	public SpaceCanvas() {
		this(new Space());
	}

	public SpaceCanvas(Space space) {
		this.space = space;

		setOpaque(true);
		setBackground(Color.WHITE);
		
		setTool(new PointMoveTool());
	}

	@Override
	protected void paintComponent(Graphics g1) {
		super.paintComponent(g1);

		Graphics2D g = (Graphics2D) g1;
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);

		for (Rendered r : space.render()) {
			r.draw(g, 1);
		}
	}

	public Space getSpace() {
		return space;
	}

	public void setSpace(Space space) {
		this.space = space;
	}

	public Tool getTool() {
		return currentTool;
	}

	public void setTool(Tool currentTool) {
		discardTool();
		this.currentTool = currentTool;
		if (currentTool != null) {
			currentTool.install(this);
		}
	}

	private void discardTool() {
		if (currentTool != null) {
			currentTool.uninstall(this);
			currentTool = null;
		}
	}

	public void refresh() {
		repaint();
	}

}