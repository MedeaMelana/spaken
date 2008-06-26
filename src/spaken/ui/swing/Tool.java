package spaken.ui.swing;

import java.awt.Graphics2D;

public interface Tool {

	public void uninstall(SpaceCanvas spaceCanvas);

	public void install(SpaceCanvas spaceCanvas);

	public void drawState(Graphics2D g);

	public String getName();

}
