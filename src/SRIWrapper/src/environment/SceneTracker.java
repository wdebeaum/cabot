package environment;

import java.util.*;

public class SceneTracker {
	
	private List<Scene> scenes;

	public SceneTracker() {
		scenes = new ArrayList<Scene>();
	}
	
	public void addScene(Scene scene)
	{
		updateMovedBlocks(scene);
		scenes.add(scene);
	}
	
	// Makes the assumption a block can't be replaced in a single frame
	private void updateMovedBlocks(Scene scene)
	{
		for (Block b : scene.integerBlockMapping.values())
		{
			boolean moved = true;
			for (Block oldBlock : getLastScene().integerBlockMapping.values())
			{
				if (b.hasSimilarPosition(oldBlock))
					moved = false;
			}
			b.setMoved(true);
		}
	}

	public Scene getLastScene()
	{
		return scenes.get(scenes.size()-1);
	}
}
