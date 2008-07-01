/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.*;

import javax.swing.JPanel;

import spaken.model.*;
import spaken.model.rendered.Rendered;

/**
 * @author Martijn van Steenbergen
 */
public class SpaceCanvas extends JPanel {

	private Space space;

	private Tool currentTool;

	private CommandHistory history;

	private AffineTransform transform;

	{
		addMouseListener(new MouseAdapter() {

			@Override
			public void mousePressed(MouseEvent e) {
				requestFocus();
			}

		});
	}

	public SpaceCanvas() {
		this(new Space(), new CommandHistory());
	}

	public SpaceCanvas(Space space, CommandHistory history) {
		this.space = space;
		this.history = history;

		setOpaque(true);
		setBackground(DrawingConstants.BACKGROUND);
	}

	@Override
	protected void paintComponent(Graphics g1) {
		super.paintComponent(g1);

		Graphics2D g = (Graphics2D) g1;
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);

		AffineTransform trans = getTransform();
		g.transform(trans);
		
		double pixelSize = getPixelSize();
		g.setStroke(new BasicStroke((float) pixelSize));

		Pos lb = new Pos(0, 0).inverseTransform(trans);
		Pos ro = new Pos(getWidth(), getHeight()).inverseTransform(trans);
		
		g.setColor(DrawingConstants.AXIS);
		Line2D line = new Line2D.Double();
		line.setLine(lb.x, 0, ro.x, 0);
		g.draw(line);
		line.setLine(0, lb.y, 0, ro.y);
		g.draw(line);

		for (Rendered r : space.render()) {
			r.draw(g, pixelSize);
		}

		if (currentTool != null) {
			currentTool.drawState(g, pixelSize);
		}
	}

	public Space getSpace() {
		return space;
	}

	public void setSpace(Space space) {
		this.space = space;
	}

	public CommandHistory getHistory() {
		return history;
	}

	public void setHistory(CommandHistory history) {
		this.history = history;
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
		refresh();
	}

	private void discardTool() {
		if (currentTool != null) {
			currentTool.uninstall(this);
			currentTool = null;
		}
	}

	public AffineTransform getTransform() {
		if (transform == null) {
			transform = new AffineTransform();
		}
		return transform;
	}

	public void refresh() {
		repaint();
	}

	public void clear() {
		space.clear();
		repaint();
	}
	
	public double getPixelSize() {
		return 1 / getTransform().getScaleX();
	}

	public double getPointSelectSize() {
		return DrawingConstants.POINT_SELECT_SIZE / getTransform().getScaleX();
	}

}
