/*
 * JLevelMeter.java
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 12 Jun 2008
 * $Id: JLevelMeter.java,v 1.1 2008/06/17 18:54:00 lgalescu Exp $
 */

package TRIPS.SpeechController;

import javax.swing.*;
import java.awt.*;

/**
 * A level meter
 */
public class JLevelMeter extends Canvas {

    public static enum MeterLayout { HORIZONTAL, VERTICAL; }; 

    private Color bgColor = Color.LIGHT_GRAY;
    private MeterLayout layout = MeterLayout.HORIZONTAL;

    private int levels = 10; // from 0 to (levels-1)

    private int maxLevel = 0;
    private int currentLevel = 0;
    
    private int width = 40;
    private int outBorder = 1;
    private int inBorder = 1;
    private int levelWidth = 16;
    private int arc = 7;
    private int length = levels*levelWidth;

    public JLevelMeter(MeterLayout layout) {
	setLayout(layout);
	show();
    }
    public JLevelMeter(MeterLayout layout, int levels) {
	this(layout);
	this.levels = levels;
    }
    public JLevelMeter(MeterLayout layout, int levels, int width, int length) {
	this(layout, levels);
	setBoxSize(width, length);
    }

    public void setBackgroundColor(Color c) {
	bgColor = c;
    }
    public void setLayout(MeterLayout layout) {
	this.layout = layout;
    }
    public void setBoxSize(int width, int length) {
	this.width = width;
	levelWidth = (int) (length / levels);
	this.length = levels*levelWidth;
	if (layout == MeterLayout.HORIZONTAL) {
	    setSize(length, width);
	} else {
	    setSize(width, length);
	}
	if (levelWidth < 6) {
	    inBorder = arc = 0;
	} else {
	    inBorder = (int) (levelWidth / 4);
	    arc = (int) (levelWidth / 2);
	}
    }

    private Color getLevelColor(int level) {
	return 
	    (level < (int) (.2 * levels)) ? Color.BLUE : 
	    (level >= (int) (.75 * levels)) ? Color.RED : Color.GREEN;
    }

    public void setLevel(int level) {
	currentLevel = level;
	if (level > maxLevel) {
	    maxLevel = level;
	}
	repaint();
    }

    public void reset() {
	currentLevel = maxLevel = 0;
	repaint();
    }

    public void paint(Graphics g) {
	// fill background
	paintBackground(g);
	// fill up to level
	for (int i = 0; i < currentLevel; i++) {
	    paintLevel(g, i, getLevelColor(i));
	}
	// mark maxLevel
	if (maxLevel > 0) {
	    paintLevel(g, maxLevel, getLevelColor(maxLevel).darker());
	} 
    }

    private void paintBackground(Graphics g) {
	g.setColor(bgColor);
	if (layout == MeterLayout.HORIZONTAL) {
	    g.fillRect(0, 0, length, width);
	} else {
	    g.fillRect(0, 0, width, length);
	}
    }

    private void paintLevel(Graphics g, int i, Color color) {
	if ((i < 0) || (i >= levels)) return;
	g.setColor(color);
	if (layout == MeterLayout.HORIZONTAL) {
	    g.fillRoundRect(i*levelWidth + inBorder, outBorder, 
			    levelWidth - 2*inBorder, width - 2*outBorder - 1, 
			    arc, arc);
	} else {
	    g.fillRoundRect(outBorder, length - (i+1)*levelWidth + inBorder, 
			    width - 2*outBorder - 1, levelWidth - 2*inBorder, 
			    arc, arc);
	}
    }
}

